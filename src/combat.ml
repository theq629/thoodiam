open Std
open Game_data

let factors_to_string ?(need_parens=true) empty_string part_to_string factors =
	let parts = List.map begin function
		| None, x -> part_to_string x
		| Some e, x -> Printf.sprintf "%s [%s]" (part_to_string x) e
		end factors in
	let parts_str = String.concat " + " parts in
	if need_parens then begin
		match parts with
		| [] -> empty_string
		| [_] when not (List.exists (function Some _, _ -> true | _ -> false) factors) -> parts_str
		| _ -> Printf.sprintf "(%s)" parts_str
	end else parts_str

module Mod_dice =
	struct
		type t =
			{
				num : (string option * int) list;
				sides : (string option * int) list;
			}

		let of_dice dice =
			{
				num = [None, dice.Dice.num];
				sides = [None, dice.Dice.sides];
			}

		let to_string mod_dice =
			Printf.sprintf "%sd%s"
				(factors_to_string "0" string_of_int mod_dice.num)
				(factors_to_string "0" string_of_int mod_dice.sides)

		let roll mod_dice rng =
			let num = List.fold_left (fun s (_, n) -> s + n) 0 mod_dice.num in
			let sides = List.fold_left (fun s (_, n) -> s + n) 0 mod_dice.sides in
			Dice.roll (Dice.make num sides) rng
	end

module Roll =
	struct
		type t =
			{
				base : (string option * int) list;
				dice : (string option * Mod_dice.t) list;
			}

		let to_string roll =
			let dice_str = factors_to_string ~need_parens:false "0" Mod_dice.to_string roll.dice in
			match roll.base with
			| [] -> dice_str
			| _ -> Printf.sprintf "%s + %s" (factors_to_string ~need_parens:false "0" string_of_int roll.base) dice_str

		let roll roll rng =
			let base = List.fold_left (fun s (_, b) -> s + b) 0 roll.base in
			let from_dice = List.fold_left (fun s (_, md) -> s + Mod_dice.roll md rng) 0 roll.dice in
			base + from_dice
	end

module Result =
	struct
		type hit_result =
			{
				damage : (Roll.t * int);
				protection : (Roll.t * int);
			}

		type t =
			{
				attack : (Roll.t * int);
				evasion : (Roll.t * int);
				hit : hit_result option;
			}

		let pairs_to_string (roll1, outcome1) (roll2, outcome2) =
			Printf.sprintf "%s vs %s -> %i vs %i" (Roll.to_string roll1) (Roll.to_string roll2) outcome1 outcome2

		let to_string result =
			(pairs_to_string result.attack result.evasion)
			^ match result.hit with
				| None -> ""
				| Some h -> "; " ^ (pairs_to_string h.damage h.protection)
	end

let d1d20 =
	Mod_dice.({
		num = [None, 1];
		sides = [None, 20];
	})

let calc_attack attacker weapon =
	let from_equip =
		List.filter_map begin fun equip_slot ->
			if equip_slot.Equip_slot.affects_combat then begin
				Opt.flat_map begin fun thing ->
					match (Thing.in_combat thing) with
					| Some ic when ic.In_combat.accuracy != 0 || (match weapon with Some w -> w == thing | None -> false) ->
						Some (Some (Thing.name thing), ic.In_combat.accuracy)
					| _ -> None
				end (in_slot attacker equip_slot)
			end else
				None
		end attacker.Being.body.Thing.kind.Thing.Kind.equip_slots in
	Roll.({
		base = (Some "melee", Being.(attacker.skills.melee))::from_equip;
		dice = [None, d1d20];
	})

let calc_evasion defender =
	let from_equip =
		List.filter_map begin fun equip_slot ->
			if equip_slot.Equip_slot.affects_combat then begin
				Opt.flat_map begin fun thing ->
					match (Thing.in_combat thing) with
					| Some ic when ic.In_combat.evasion != 0 || equip_slot.Equip_slot.is_armour ->
						Some (Some (Thing.name thing), ic.In_combat.evasion)
					| _ -> None
				end (in_slot defender equip_slot)
			end else
				None
		end defender.Being.body.Thing.kind.Thing.Kind.equip_slots in
	Roll.({
		base = (Some "evasion", Being.(defender.skills.melee))::from_equip;
		dice = [None, d1d20];
	})

let calc_protection defender =
	let armour_dice =
		List.filter_map begin fun equip_slot ->
			Opt.flat_map begin fun thing ->
				match thing.Thing.kind.Thing.Kind.armour with
				| None -> None
				| Some stats ->
					Some (Some thing.Thing.kind.Thing.Kind.name, Mod_dice.of_dice stats.Armour.protection)
			end (in_slot defender equip_slot)
		end defender.Being.body.Thing.kind.Thing.Kind.equip_slots in
	Roll.({
		base = [];
		dice = armour_dice;
	})

let calc_damage attacker (weapon : Thing.t option) =
	let weapon_damage, weapon_weight =
		match weapon with
		| None -> Dice.(make 0 0), 0.
		| Some weapon ->
			begin match Thing.(melee weapon) with
			| Some c -> c.Weapon.damage, Thing.(weight weapon)
			| None -> Dice.(make 0 0), 0.
			end in
	let strength_bonus =
		match Thing.(bodyable Being.(body attacker)) with
		| None -> 0
		| Some b -> min (round_to_int weapon_weight) Bodyable.(b.str) in
	let damage =
		Mod_dice.({
			num = [None, weapon_damage.Dice.num];
			sides = [None, weapon_damage.Dice.sides; Some "strength", strength_bonus];
		}) in
	Roll.({
		base = [];
		dice = [None, damage];
	})

let melee_combat attacker defender rng =
	let weapon =
		let slots = List.filter (fun es -> es.Equip_slot.is_melee) attacker.Being.body.Thing.kind.Thing.Kind.equip_slots in
		let weapons = List.filter_map (in_slot attacker) slots in
		match weapons with
		| [] -> None
		| _ -> Some (Rng.Uniform.list_elt weapons rng) in
	let attack_roll = calc_attack attacker weapon in
	let evasion_roll = calc_evasion defender in
	let attack_value = Roll.roll attack_roll rng in
	let evasion_value = Roll.roll evasion_roll rng in
	if evasion_value >= attack_value then begin
		let result = Result.({
				attack = (attack_roll, attack_value);
				evasion = (evasion_roll, evasion_value);
				hit = None;
			}) in
		None, result
	end else begin
		let damage_roll = calc_damage attacker weapon in
		let protection_roll = calc_protection defender in
		let damage_value = Roll.roll damage_roll rng in
		let protection_value = Roll.roll protection_roll rng in
		let result = Result.({
				attack = (attack_roll, attack_value);
				evasion = (evasion_roll, evasion_value);
				hit = Some {
					damage = (damage_roll, damage_value);
					protection = (protection_roll, protection_value);
				};
			}) in
		Some (max 0 (damage_value - protection_value)), result
	end
