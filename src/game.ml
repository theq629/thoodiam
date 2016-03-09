open Std
module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil
open Game_data

type time = float

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

module Message =
	struct
		type t =
			| Melee_hit of (Being.t * Being.t * int)
			| Melee_miss of (Being.t * Being.t)
			| Pick_up of (Being.t * Thing.t)
			| Drop of (Being.t * Thing.t)
			| Equip of (Being.t * Equip_slot.t * Thing.t)
			| Unequip of (Being.t * Equip_slot.t * Thing.t)
			| Die of Being.t
	end

type dir = N | S | E | W | NE | NW | SE | SW

type command =
	| Move of dir
	| Melee_attack of dir
	| Pick_up of Thing.t
	| Drop of Thing.t
	| Equip of (Thing.t * Equip_slot.t)
	| Unequip of Equip_slot.t
	| Quit

module Action_queue = CCHeap.Make(
	struct
		type t = (time * Being.t * command)
		let leq (t1, _, _) (t2, _, _) = t1 < t2
	end)

type t =
	{
		rng : Rng.Source.t;
		map : Cell.t Map.t;
		mutable time : time;
		mutable beings : Being.t list;
		mutable action_queue : Action_queue.t;
		mutable player : Being.t option;
		player_seen : tile option Map.t;
		player_fov : bool Map.t;
		mutable messages : Message.t list;
	}

let dir_to_vec = function
	| N -> (0, -1)
	| S -> (0, 1)
	| E -> (1, 0)
	| W -> (-1, 0)
	| NE -> (1, -1)
	| NW -> (-1, -1)
	| SE -> (1, 1)
	| SW -> (-1, 1)

let vec_to_dir = function
	| (0, -1) -> Some N
	| (0, 1) -> Some S
	| (1, 0) -> Some E
	| (-1, 0) -> Some W
	| (1, -1) -> Some NE
	| (-1, -1) -> Some NW
	| (1, 1) -> Some SE
	| (-1, 1) -> Some SW
	| _ -> None

let round_to_int x = int_of_float (0.5 +. x)

let remove_from_list xs x =
	let found = ref false in
	let xs1 = List.filter begin fun y ->
			if x = y then begin
				found := true;
				false
			end else
				true
		end xs in
	(* TODO: faster? *)
	!found, xs1

let add_thing game p thing =
	let cell = Map.get game.map p in
	cell.Cell.things <- thing::cell.Cell.things

let remove_thing game p thing =
	let cell = Map.get game.map p in
	let found, things1 = remove_from_list cell.Cell.things thing in
	cell.Cell.things <- things1;
	found

let place_being game being at =
	let found = remove_thing game being.Being.at being.Being.body in
	add_thing game at being.Being.body;
	being.Being.at <- at;
	found

let init_being game body_kind skills at =
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
	ignore (place_being game being being.Being.at);
	game.beings <- being::game.beings;
	being

let remove_being game being =
	let found_thing = remove_thing game being.Being.at being.Being.body in
	let found_being, beings1 = remove_from_list game.beings being in
	game.beings <- beings1;
	begin match game.player with
	| None -> ()
	| Some b when b == being ->
		game.player <- None
	| _ -> ()
	end;
	found_thing && found_being

let in_slot being es =
	try Some (List.assoc es Game_data.(being.Being.equip))
	with Not_found -> None

let add_msg game at msg =
	(* TODO: check if player can see location *)
	game.messages <- msg::game.messages

let handle_combat game attacker defender rng =
	let open Being in
	let open Bodyable in
	let d1d20 = Dice.make 1 20 in
	let attack_roll = attacker.skills.melee + (Dice.roll d1d20 rng) in
	let evasion_roll = defender.skills.evasion + (Dice.roll d1d20 rng) in
	if attack_roll > evasion_roll then begin
		let a_str =
			match attacker.body.Thing.kind.Thing.Kind.bodyable with
			| None -> 0
			| Some b -> b.str in
		let a_wpn =
			let wpn_slots = List.filter (fun es -> es.Equip_slot.is_melee) attacker.Being.body.Thing.kind.Thing.Kind.equip_slots in
			let wpns = List.map (in_slot attacker) wpn_slots in
			Rng.Uniform.list_elt wpns rng in
		(* TODO: handle unarmed combat and combat with non-meleeable things *)
		Opt.iter begin fun a_wpn ->
			Opt.iter begin fun a_wpn_cbt ->
				let d_amrs =
					let amr_slots = List.filter (fun es -> es.Equip_slot.is_armour) attacker.Being.body.Thing.kind.Thing.Kind.equip_slots in
					List.filter_map (in_slot attacker) amr_slots in
				let a_extra_dice = (attack_roll - evasion_roll) / (7 + round_to_int a_wpn.Thing.kind.Thing.Kind.weight) in
				let damage_roll = Dice.roll Combat.(Dice.(make (a_wpn_cbt.damage.num + a_extra_dice) (a_wpn_cbt.damage.sides + max (round_to_int a_wpn.Thing.kind.Thing.Kind.weight) a_str))) rng in
			ignore (a_str, a_wpn, d_amrs);
				let protection_roll =
					List.fold_left begin fun r a ->
						r + match a.Thing.kind.Thing.Kind.armour with
							| Some a -> Dice.roll a.Combat.protection rng
							| None -> 0
					end 0 d_amrs in
				let hp = max 0 (damage_roll - protection_roll) in
				add_msg game attacker.Being.at (Message.Melee_hit (attacker, defender, hp));
				defender.hp <- defender.hp - hp;
				if defender.hp <= 0 then begin
					add_msg game defender.Being.at (Message.Die defender);
					ignore (remove_being game defender)
				end
			end a_wpn.Thing.kind.Thing.Kind.melee
		end a_wpn
	end else begin
		add_msg game attacker.Being.at (Message.Melee_miss (attacker, defender))
	end

let update_vision game player =
	let set_visible p =
		if Map.is_valid game.map p then begin
			Map.set game.player_fov p true;
			let cell = Map.get game.map p in
			let tile =
				let rec run =
					function
					| [] -> cell.Cell.terrain.Terrain.tile
					| [t] -> Thing.(Kind.(t.kind.tile))
					| t::_ when Thing.(Kind.(t.kind.visual_priority)) -> Thing.(Kind.(t.kind.tile))
					| _::ts1 -> run ts1 in
				run cell.Cell.things in
			Map.set game.player_seen p (Some tile)
		end in
	let blocks_sight p =
		not (Map.is_valid game.map p)
		|| (Map.get game.map p).Cell.terrain.Terrain.blocking in
	Map.update game.player_fov (fun _ _ -> false);
	Opt.iter begin fun bodyable ->
		set_visible player.Being.at;
		Fov.compute blocks_sight set_visible player.Being.at bodyable.Bodyable.vision
	end player.Being.body.Thing.kind.Thing.Kind.bodyable

let init map fov_radius configure rng =
	let game = {
			rng = rng;
			map = map;
			time = 0.;
			player = None;
			beings = [];
			action_queue = Action_queue.empty;
			player_seen = Map.map map (fun _ v -> None);
			player_fov = Map.map map (fun _ _ -> false);
			messages = [];
		} in
	configure game;
	game

let set_player game being =
	game.player <- Some being;
	update_vision game being

let handle_command game being cmd =
	begin match cmd with
	| Quit -> begin
			add_msg game being.Being.at (Message.Die being);
			ignore (remove_being game being)
		end
	| Move dir -> begin
			let p1 = Vec.(being.Being.at + dir_to_vec dir) in
			if (Map.is_valid game.map p1)
				&& not (Map.get game.map p1).Cell.terrain.Terrain.blocking then begin
				ignore (place_being game being p1)
			end
		end
	| Melee_attack dir -> begin
			let p1 = Vec.(being.Being.at + dir_to_vec dir) in
			List.iter begin fun being1 ->
				if being1.Being.at = p1 then begin
					handle_combat game being being1 game.rng
				end
			end game.beings
		end
	| Pick_up thing -> begin
			let new_weight = being.Being.inv_weight +. thing.Thing.kind.Thing.Kind.weight in
			if new_weight <= being.Being.can_carry then begin
				let found = remove_thing game being.Being.at thing in
				if found then begin
					add_msg game being.Being.at (Message.Pick_up (being, thing));
					being.Being.inv <- thing::being.Being.inv;
					being.Being.inv_weight <- being.Being.inv_weight +. thing.Thing.kind.Thing.Kind.weight
				end
			end
		end
	| Drop thing -> begin
			let found, inv1 = remove_from_list being.Being.inv thing in
			if found then begin
				add_msg game being.Being.at (Message.Drop (being, thing));
				being.Being.inv <- inv1;
				add_thing game being.Being.at thing;
				being.Being.inv_weight <- being.Being.inv_weight -. thing.Thing.kind.Thing.Kind.weight
			end
		end
	| Equip (thing, equip_slot) -> begin
			if List.mem equip_slot being.Being.body.Thing.kind.Thing.Kind.equip_slots && not (List.mem_assoc equip_slot being.Being.equip) then begin
				let found, inv1 = remove_from_list being.Being.inv thing in
				if found then begin
					add_msg game being.Being.at (Message.Equip (being, equip_slot, thing));
					being.Being.inv <- inv1;
					being.Being.equip <- (equip_slot, thing)::being.Being.equip
				end
			end
		end
	| Unequip equip_slot -> begin
			try
				let thing = List.assoc equip_slot being.Being.equip in
				add_msg game being.Being.at (Message.Unequip (being, equip_slot, thing));
				being.Being.equip <- List.remove_assoc equip_slot being.Being.equip;
				being.Being.inv <- thing :: being.Being.inv
			with Not_found -> ()
		end
	end;
	Opt.iter (update_vision game) game.player

let time_for_action being cmd =
	let base_time = 1. in
	let diag_time = sqrt (base_time**2. +. base_time**2.) in
	match cmd with
	| Move dir ->
		begin match dir with
		| NW | NE | SW | SE -> diag_time
		| _ -> base_time
		end
	| _ -> base_time

let queue_action game being cmd =
	let need_time = time_for_action being cmd in
	game.action_queue <- Action_queue.add game.action_queue (game.time +. need_time, being, cmd)

let update_ai game =
	Opt.iter begin fun player ->
		List.iter begin fun being ->
			if being != player then
				Opt.iter begin fun dir ->
					queue_action game being (Melee_attack dir)
				end (vec_to_dir Vec.(player.Being.at - being.Being.at))
		end game.beings
	end game.player

let update game cmds =
	game.messages <- [];
	update_ai game;
	begin match game.player with
	| None -> ()
	| Some being ->
		List.iter begin fun cmd ->
			queue_action game being cmd
		end cmds
	end;
	while not (Action_queue.is_empty game.action_queue) do
		let queue1, (time, being, action) = Action_queue.take_exn game.action_queue in
		game.time <- time;
		game.action_queue <- queue1;
		handle_command game being action;
		Printf.eprintf "game time %f\n" game.time
	done
