open Std
module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil
open Game_data
open Game_state
open Game_changes

type time = float

type event =
	| Tick
	| Being_death of Being.t
	| Being_action of Being.t
	| Take_stairs of (Being.t * stairs_dir)

module Event_queue = CCHeap.Make(
	struct
		type t = (time * event)
		let leq (t1, _) (t2, _) = t1 < t2
	end)

module Cell =
	struct
		type t =
			{
				mutable terrain : Terrain.t;
				mutable things : Thing.t list;
			}

		let make terrain =
			{
				terrain = terrain;
				things = [];
			}
	end

let time_for_action actions =
	let base_time = 1. in
	let diag_time = sqrt (base_time**2. +. base_time**2.) in
	Action.(match actions with
	| Move dir ->
		Direction.(begin match dir with
		| NW | NE | SW | SE -> diag_time
		| _ -> base_time
		end)
	| _ -> base_time
	)

type t =
	{
		map : Cell.t Map.t;
		up_stairs : Map.Location.t list;
		down_stairs : Map.Location.t list;
		mutable time : time;
		mutable beings : Being.t list;
		mutable event_queue : Event_queue.t;
		mutable messages : Message.t list;
	}

let add_msg region at msg =
	(* TODO: handle message locations so we can see if the player can see the event *)
	region.messages <- msg::region.messages

let queue_event region time event =
	region.event_queue <- Event_queue.add region.event_queue (time, event)

let init map up_stairs down_stairs configure =
	let region = {
			map = map;
			up_stairs = up_stairs;
			down_stairs = down_stairs;
			time = 0.;
			beings = [];
			event_queue = Event_queue.empty;
			messages = [];
		} in
	configure region;
	queue_event region 0. Tick;
	region

let revisit region =
	region.messages <- []

let add_thing region p thing =
	let cell = Map.get region.map p in
	cell.Cell.things <- thing::cell.Cell.things

let remove_thing region p thing =
	let cell = Map.get region.map p in
	let found, things1 = remove_from_list_q cell.Cell.things thing in
	cell.Cell.things <- things1;
	found

let place_being region being at =
	let found = remove_thing region being.Being.at being.Being.body in
	add_thing region at being.Being.body;
	being.Being.at <- at;
	found

let have_thing region p thing =
	let cell = Map.get region.map p in
	List.memq thing cell.Cell.things

let init_being region body_kind at =
	let being = Being.make body_kind at in
	ignore (place_being region being being.Being.at);
	region.beings <- being::region.beings;
	queue_event region region.time (Being_action being);
	being

let remove_being region being =
	let found_thing = remove_thing region being.Being.at being.Being.body in
	let found_being, beings1 = remove_from_list_q region.beings being in
	region.event_queue <- Event_queue.filter (function (_, Being_action b) -> b != being | _ -> true) region.event_queue;
	region.beings <- beings1;
	found_thing && found_being

let transfer_being region1 being region2 at =
	if remove_being region1 being then begin
		region2.beings <- being::region2.beings;
		add_thing region2 at being.Being.body;
		being.Being.at <- at;
		queue_event region2 region2.time (Being_action being);
		true
	end else
		false

let kill_being region being =
	Being.(
		add_msg region being.at (Message.Die being);
		ignore (remove_being region being);
		queue_event region region.time (Being_death being);
		List.iter begin fun thing ->
			add_thing region being.at thing
		end Being.(inv being);
		List.iter begin fun (_, thing) ->
			add_thing region being.at thing
		end being.equip;
	)

let handle_combat_result region attacker defender hit result =
	match hit with
	| None ->
		add_msg region attacker.Being.at (Message.Miss (attacker, defender, result))
	| Some hp ->
		Being.(
			add_msg region attacker.Being.at (Message.Hit (attacker, defender, hp, result));
			defender.hp <- defender.hp - hp;
			if defender.hp <= 0 then
				kill_being region defender;
			attacker.stress <- 10;
			defender.stress <- 10
		)

let handle_action region being action rng =
	let open Action in
	begin match action with
	| Quit -> begin
			add_msg region being.Being.at (Message.Die being);
			ignore (kill_being region being);
			true
		end
	| Wait -> begin
			false
		end
	| Move dir -> begin
			let p1 = Vec.(being.Being.at + Direction.to_vec dir) in
			let cell = Map.get region.map p1 in
			if (Map.is_valid region.map p1)
				&& not cell.Cell.terrain.Terrain.blocking
				&& not (List.exists Thing.blocks cell.Cell.things) then begin
				ignore (place_being region being p1)
			end;
			false
		end
	| Melee_attack dir -> begin
			let p1 = Vec.(being.Being.at + Direction.to_vec dir) in
			begin match List.find_pred (fun b -> b.Being.at = p1) region.beings with
			| None -> ()
			| Some being1 ->
				let hit, result = Combat.melee_combat being being1 rng in
				handle_combat_result region being being1 hit result
			end;
			false
		end;
	| Thrown_attack (thing, target, target_being) -> begin
			(* TODO: it would probably be better if we didn't get the being from the UI, but it's location may change before we process the event *)
			let range = Vec.(dist (float_of_int being.Being.at) (float_of_int target)) in
			if range < Being.throw_range being thing && Being.have being thing then begin
				ignore (Being.lose being thing);
				add_thing region target thing;
				let hit, result = Combat.throw_combat being target_being thing range rng in
				handle_combat_result region being target_being hit result
			end;
			false
		end;
	| Pick_up thing -> begin
			let found = have_thing region being.Being.at thing in
			if found && Being.get being thing && remove_thing region being.Being.at thing then
				add_msg region being.Being.at (Message.Pick_up (being, thing));
			false
		end
	| Drop thing -> begin
			if Being.lose being thing then begin
				add_msg region being.Being.at (Message.Drop (being, thing));
				add_thing region being.Being.at thing
			end;
			false
		end
	| Equip (thing, equip_slot) -> begin
			if Being.equip being equip_slot thing then
				add_msg region being.Being.at (Message.Equip (being, equip_slot, thing));
			false
		end
	| Unequip equip_slot -> begin
			begin match Being.unequip being equip_slot with
			| Some thing -> add_msg region being.Being.at (Message.Unequip (being, equip_slot, thing))
			| None -> ()
			end;
			false
		end
	| Take_stairs dir-> begin
			let stairs_list =
				match dir with
				| Up -> region.up_stairs
				| Down -> region.down_stairs in
			if List.mem being.Being.at stairs_list then begin
				add_msg region being.Being.at (Message.Take_stairs (being, dir));
				queue_event region region.time (Take_stairs (being, dir));
				true
			end else
				false
		end
	end

let tick region =
	List.iter begin fun being ->
		let open Being in
		if being.stress > 0 then
			being.stress <- being.stress - 1
		else
			being.hp <- min being.max_hp (being.hp + 1)
	end region.beings;
	queue_event region (region.time +. 1.) Tick

let update region being_ai being_death take_stairs rng =
	region.messages <- [];
	let rec run () =
		match Event_queue.take region.event_queue with
		| None -> ()
		| Some (queue1, (time, event)) ->
			region.time <- time;
			region.event_queue <- queue1;
			let continue =
				begin match event with
				| Tick ->
					tick region;
					true
				| Being_death being ->
					being_death being
				| Being_action being ->
					let action, continue = being_ai being in
					let deferred = handle_action region being action rng in
					if deferred then begin
						true
					end else begin
						queue_event region (region.time +. time_for_action action) (Being_action being);
						continue
					end
				| Take_stairs (being, dir) ->
					take_stairs being dir
				end in
			if continue then run ()
			else () in
	run ()
