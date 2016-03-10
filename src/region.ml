open Std
module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil
open Game_data

type time = float

module Action_queue = CCHeap.Make(
	struct
		type t = (time * Being.t * Action.t)
		let leq (t1, _, _) (t2, _, _) = t1 < t2
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

let time_for_action being actions =
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
		mutable action_queue : Action_queue.t;
		mutable messages : Message.t list;
	}

let init map configure =
	let region = {
			map = map;
			time = 0.;
			beings = [];
			action_queue = Action_queue.empty;
			messages = [];
		} in
	configure region;
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
	let body = Thing.make body_kind in
	let scale_stat base scale = 0.5 +. base *. 1.2**(float_of_int scale) in
	let max_hp, can_carry =
		match body.Thing.kind.Thing.Kind.bodyable with
		| None -> 0, 0.
		| Some b ->
			Bodyable.(
				round_to_int (scale_stat 10. b.con),
				scale_stat 20. b.str
			) in
	let being = Being.({
			at = at;
			skills = skills;
			body = body;
			inv = [];
			equip = [];
			can_carry = can_carry;
			inv_weight = 0.;
			max_hp = max_hp;
			hp = max_hp;
		}) in
	ignore (place_being region being being.Being.at);
	region.beings <- being::region.beings;
	being

let remove_being region being =
	let found_thing = remove_thing region being.Being.at being.Being.body in
	let found_being, beings1 = remove_from_list region.beings being in
	region.beings <- beings1;
	found_thing && found_being

let add_msg region at msg =
	(* TODO: handle message locations so we can see if the player can see the event *)
	region.messages <- msg::region.messages

let queue_action region being actions =
	let need_time = time_for_action being actions in
	region.action_queue <- Action_queue.add region.action_queue (region.time +. need_time, being, actions)

let handle_combat region attacker defender rng =
	match Combat_system.melee_combat region attacker defender rng with
	| None ->
		add_msg region attacker.Being.at (Message.Melee_miss (attacker, defender))
	| Some hp ->
		Being.(
			add_msg region attacker.Being.at (Message.Melee_hit (attacker, defender, hp));
			defender.hp <- defender.hp - hp;
			if defender.hp <= 0 then begin
				add_msg region defender.Being.at (Message.Die defender);
				ignore (remove_being region defender)
			end
		)

let handle_action region being action rng =
	let open Action in
	begin match action with
	| Quit -> begin
			add_msg region being.Being.at (Message.Die being);
			ignore (remove_being region being)
		end
	| Move dir -> begin
			let p1 = Vec.(being.Being.at + Direction.to_vec dir) in
			if (Map.is_valid region.map p1)
				&& not (Map.get region.map p1).Cell.terrain.Terrain.blocking then begin
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

let update region rng =
	region.messages <- [];
	(* TODO: actually enforce being action times instead of handling all actions *)
	while not (Action_queue.is_empty region.action_queue) do
		let queue1, (time, being, action) = Action_queue.take_exn region.action_queue in
		region.time <- time;
		region.action_queue <- queue1;
		handle_action region being action rng;
		Printf.eprintf "region time %f\n" region.time
	done
