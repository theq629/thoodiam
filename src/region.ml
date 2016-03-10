open Std
module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil
open Game_data
open Game_state

type time = float

type event =
	| Tick
	| Being_action of Being.t

module Event_queue = CCHeap.Make(
	struct
		type t = (time * event)
		let leq (t1, _) (t2, _) = t1 < t2
	end)

module Cell =
	struct
		type t =
			{
				terrain : Terrain.t;
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

let init map configure =
	let region = {
			map = map;
			time = 0.;
			beings = [];
			event_queue = Event_queue.empty;
			messages = [];
		} in
	configure region;
	queue_event region 0. Tick;
	region

let add_thing region p thing =
	let cell = Map.get region.map p in
	cell.Cell.things <- thing::cell.Cell.things

let remove_thing region p thing =
	let cell = Map.get region.map p in
	let found, things1 = remove_from_list cell.Cell.things thing in
	cell.Cell.things <- things1;
	found

let place_being region being at =
	let found = remove_thing region being.Being.at being.Being.body in
	add_thing region at being.Being.body;
	being.Being.at <- at;
	found

let init_being region body_kind skills at =
	let being_body = Thing.make body_kind in
	let scale_stat base scale = 0.5 +. base *. 1.2**(float_of_int scale) in
	let max_hp, can_carry =
		match being_body.Thing.kind.Thing.Kind.bodyable with
		| None -> 0, 0.
		| Some b ->
			Bodyable.(
				round_to_int (scale_stat 10. b.con),
				scale_stat 20. b.str
			) in
	let being = Being.({
			at = at;
			skills = skills;
			body = being_body;
			inv = [];
			equip = [];
			can_carry = can_carry;
			inv_weight = 0.;
			max_hp = max_hp;
			hp = max_hp;
			stress = 0;
		}) in
	ignore (place_being region being being.Being.at);
	region.beings <- being::region.beings;
	queue_event region region.time (Being_action being);
	being

let remove_being region being =
	let found_thing = remove_thing region being.Being.at being.Being.body in
	let found_being, beings1 = remove_from_list region.beings being in
	region.beings <- beings1;
	found_thing && found_being

let handle_combat region attacker defender rng =
	let hit, result = Combat_system.melee_combat attacker defender rng in
	match hit with
	| None ->
		add_msg region attacker.Being.at (Message.Melee_miss (attacker, defender, result))
	| Some hp ->
		Being.(
			add_msg region attacker.Being.at (Message.Melee_hit (attacker, defender, hp, result));
			defender.hp <- defender.hp - hp;
			if defender.hp <= 0 then begin
				add_msg region defender.Being.at (Message.Die defender);
				ignore (remove_being region defender)
			end;
			attacker.stress <- 10;
			defender.stress <- 10
		)

let handle_action region being action rng =
	let open Action in
	begin match action with
	| Quit -> begin
			add_msg region being.Being.at (Message.Die being);
			ignore (remove_being region being)
		end
	| Wait ->
		()
	| Move dir -> begin
			let p1 = Vec.(being.Being.at + Direction.to_vec dir) in
			let cell = Map.get region.map p1 in
			if (Map.is_valid region.map p1)
				&& not cell.Cell.terrain.Terrain.blocking
				&& not (List.exists (fun t -> t.Thing.kind.Thing.Kind.visual_priority) cell.Cell.things) then begin
				ignore (place_being region being p1)
			end
		end
	| Melee_attack dir -> begin
			let p1 = Vec.(being.Being.at + Direction.to_vec dir) in
			match List.find_pred (fun b -> b.Being.at = p1) region.beings with
			| None -> ()
			| Some being1 ->
				handle_combat region being being1 rng
		end
	| Pick_up thing -> begin
			let new_weight = being.Being.inv_weight +. thing.Thing.kind.Thing.Kind.weight in
			if new_weight <= being.Being.can_carry then begin
				let found = remove_thing region being.Being.at thing in
				if found then begin
					add_msg region being.Being.at (Message.Pick_up (being, thing));
					being.Being.inv <- thing::being.Being.inv;
					being.Being.inv_weight <- being.Being.inv_weight +. thing.Thing.kind.Thing.Kind.weight
				end
			end
		end
	| Drop thing -> begin
			let found, inv1 = remove_from_list being.Being.inv thing in
			if found then begin
				add_msg region being.Being.at (Message.Drop (being, thing));
				being.Being.inv <- inv1;
				add_thing region being.Being.at thing;
				being.Being.inv_weight <- being.Being.inv_weight -. thing.Thing.kind.Thing.Kind.weight
			end
		end
	| Equip (thing, equip_slot) -> begin
			if List.mem equip_slot being.Being.body.Thing.kind.Thing.Kind.equip_slots && not (List.mem_assoc equip_slot being.Being.equip) then begin
				let found, inv1 = remove_from_list being.Being.inv thing in
				if found then begin
					add_msg region being.Being.at (Message.Equip (being, equip_slot, thing));
					being.Being.inv <- inv1;
					being.Being.equip <- (equip_slot, thing)::being.Being.equip
				end
			end
		end
	| Unequip equip_slot -> begin
			try
				let thing = List.assoc equip_slot being.Being.equip in
				add_msg region being.Being.at (Message.Unequip (being, equip_slot, thing));
				being.Being.equip <- List.remove_assoc equip_slot being.Being.equip;
				being.Being.inv <- thing :: being.Being.inv
			with Not_found -> ()
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

let update region being_ai rng =
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
				| Being_action being ->
					let action, continue = being_ai being in
					handle_action region being action rng;
					queue_event region (region.time +. time_for_action action) (Being_action being);
					continue
				end in
			if continue then run ()
			else () in
	run ()
