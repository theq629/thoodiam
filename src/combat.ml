open Std
open Game_data
open Game_state

let factors_to_desc ?(need_parens=true) empty_string part_to_string factors =
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

let factors_to_string ?(need_parens=true) empty_string part_to_string factors =
	let parts = List.map begin function
		| None, x -> part_to_string x
		| Some _, x -> Printf.sprintf "%s" (part_to_string x)
		end factors in
	let parts_str = String.concat " + " parts in
	if need_parens then begin
		match parts with
		| [] -> empty_string
		| [_] when not (List.exists (function Some _, _ -> true | _ -> false) factors) -> parts_str
		| _ -> Printf.sprintf "(%s)" parts_str
	end else parts_str

let int_factors_to_string factors =
	string_of_int (List.fold_left (fun t (_, x) -> t + x) 0 factors)

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

		let to_desc mod_dice =
			Printf.sprintf "%sd%s"
				(factors_to_desc "0" string_of_int mod_dice.num)
				(factors_to_desc "0" string_of_int mod_dice.sides)

		let to_string mod_dice =
			Printf.sprintf "%sd%s"
				(int_factors_to_string mod_dice.num)
				(int_factors_to_string mod_dice.sides)

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

		let to_desc roll =
			let dice_str = factors_to_desc ~need_parens:false "0" Mod_dice.to_desc roll.dice in
			match roll.base with
			| [] -> dice_str
			| _ -> Printf.sprintf "%s + %s" (factors_to_desc ~need_parens:false "0" string_of_int roll.base) dice_str

		let to_string roll =
			let dice_str = String.concat " + " (List.map (fun (_, d) -> Mod_dice.to_string d) roll.dice) in
			match roll.base with
			| [] -> dice_str
			| _ -> Printf.sprintf "%s + %s" (int_factors_to_string roll.base) dice_str

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

		let pairs_to_desc (roll1, outcome1) (roll2, outcome2) =
			Printf.sprintf "%s vs %s -> %i vs %i" (Roll.to_desc roll1) (Roll.to_desc roll2) outcome1 outcome2

		let to_desc result =
			(pairs_to_desc result.attack result.evasion)
			^ match result.hit with
				| None -> ""
				| Some h -> "; " ^ (pairs_to_desc h.damage h.protection)

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

let effective_weapon_equip ?(weight_div=1.) ~skill_label ~get_skill ~get_weapon_stats being equips ?weapon_slot weapon =
	let accuracy_from_equip =
		List.filter_map begin fun (_, thing) ->
			match Thing.in_combat thing with
			| Some ic when ic.In_combat.accuracy != 0 || thing == weapon ->
				Some (Some (Thing.name thing), ic.In_combat.accuracy)
			| _ -> None
		end equips in
	let strength =
		match Thing.(bodyable Being.(body being)) with
		| None -> 0
		| Some b -> Bodyable.(b.str) in
	let make_dice stats strength_label weight strength_extra =
		let strength_bonus = min (int_of_float weight) strength in
		let strength_factors =
			if strength_bonus != 0 then [Some strength_label, strength_bonus]
			else [] in
		Mod_dice.(Weapon.({
			num = [None, stats.damage.Dice.num];
			sides = (None, stats.damage.Dice.sides)::(strength_factors @ strength_extra);
		})) in
	let weight = Thing.weight weapon /. weight_div in
	let damage =
		match get_weapon_stats weapon, weapon_slot with
		| None, _ ->
			Some "no weapon", Mod_dice.of_dice Dice.zero
		| Some stats, Some weapon_slot ->
			Being.(match weapon_state being weapon_slot with
			| No_weapon ->
				Some "no weapon", Mod_dice.of_dice Dice.zero
			| Need_hand ->
				Some "can't use weapon", Mod_dice.of_dice Dice.zero
			| One_handed ->
				Some Thing.(name weapon), make_dice stats "strength" weight []
			| Two_handed when Weapon.(stats.handedness = Hand_and_a_half) ->
				Some Thing.(name weapon), make_dice stats "strength" weight [Some "hand-and-a-half", 2]
			| Two_handed ->
				let use_weight = Thing.weight weapon /. 1.5 in
				Some Thing.(name weapon), make_dice stats "two-handed strength" use_weight []
			)
		| Some stats, _ ->
			Some Thing.(name weapon), make_dice stats "strength" weight [] in
	(
		(Some skill_label, get_skill being)::accuracy_from_equip,
		damage,
		get_weapon_stats weapon
	)

let no_weapon =
	(
		[Some "no weapon", 0],
		(Some "no weapon", Mod_dice.of_dice Dice.zero)
	)

let effective_weapon being weapon_slot =
	(* TODO: Where should we decide what kind of weapon it is? Probably in the UI code. *)
	match Being.in_slot being weapon_slot with
	| None -> no_weapon
	| Some weapon ->
		let wa, wd, _ = effective_weapon_equip
			~skill_label:"melee"
			~get_skill:(fun b -> Being.(b.skills.Skills.melee))
			~get_weapon_stats:Thing.melee
			being (Being.get_usable_equips being) ~weapon_slot weapon in
		wa, wd

let calc_attack attacker weapon_accuracy =
	Roll.({
		base = weapon_accuracy;
		dice = [None, d1d20];
	})

let calc_evasion defender equips =
	let from_equip =
		List.filter_map begin fun (_, thing) ->
			match (Thing.in_combat thing) with
			| Some ic when ic.In_combat.evasion != 0 || Thing.is_armour thing ->
				Some (Some (Thing.name thing), ic.In_combat.evasion)
			| _ -> None
		end equips in
	Roll.({
		base = (Some "evasion", Being.(defender.skills.Skills.evasion))::from_equip;
		dice = [None, d1d20];
	})

let calc_damage (weapon_damage_label, weapon_damage) num_bonuses side_bonuses =
	Roll.({
		base = [];
		dice =
			[
				weapon_damage_label,
				Mod_dice.({
					num = weapon_damage.num @ num_bonuses;
					sides = weapon_damage.sides @ side_bonuses;
				})
			]
	})

let calc_protection equips =
	let armour_dice =
		List.flat_map begin fun (_, thing) ->
			List.filter_map begin
				function
				| None -> None
				| Some stats ->
					Some (Some Thing.(name thing), Mod_dice.of_dice stats.Armour.protection)
			end Thing.([body_armour thing; shield thing; helm thing])
		end equips in
	Roll.({
		base = [];
		dice =
			match armour_dice with
			| [] -> [Some "no armour", Mod_dice.of_dice Dice.zero]
			| _ -> armour_dice;
	})

let combat ~attacker ~defender ~attacker_equips ~defender_equips ~weapon ~weapon_accuracy ~weapon_damage ?(side_bonuses=[]) rng =
	let attack_roll = calc_attack attacker weapon_accuracy in
	let evasion_roll = calc_evasion defender defender_equips in
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
		let dice_num_bonuses =
			match weapon with
			| None -> []
			| Some weapon ->
				let crit_fac = 7. +. Thing.weight weapon in
				let crits = int_of_float (float_of_int (attack_value - evasion_value) /. crit_fac) in
				if crits > 1 then [Some "criticals", crits]
				else if crits == 1 then [Some "critical", crits]
				else [] in
		let damage_roll = calc_damage weapon_damage dice_num_bonuses side_bonuses in
		let protection_roll = calc_protection defender_equips in
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

let melee_combat attacker defender rng =
	let attacker_equips = Being.get_usable_equips attacker in
	let defender_equips = Being.get_usable_equips defender in
	let effective_weapon_equip = effective_weapon_equip
			~skill_label:"melee"
			~get_skill:(fun b -> Being.(b.skills.Skills.melee))
			~get_weapon_stats:Thing.melee in
	let weapon, (weapon_accuracy, weapon_damage) =
		let weapons =
			List.filter_map begin fun (slot, thing) ->
				if Thing.is_melee thing then Some (slot, thing)
				else None
			end attacker_equips in
		match weapons with
		| [] ->
			None, no_weapon
		| _ ->
			let weapon_slot, weapon = Rng.Uniform.list_elt weapons rng in
			let weapon_accuracy, weapon_damage, _ = effective_weapon_equip attacker attacker_equips ~weapon_slot weapon in
			Some weapon, (weapon_accuracy, weapon_damage) in
	combat ~attacker ~defender ~attacker_equips ~defender_equips ~weapon ~weapon_accuracy ~weapon_damage rng

let throw_combat attacker defender weapon range rng =
	let attacker_equips = Being.get_usable_equips attacker in
	let defender_equips = Being.get_usable_equips defender in
	let effective_weapon_equip = effective_weapon_equip
			~weight_div:0.5
			~skill_label:"melee"
			~get_skill:(fun b -> Being.(b.skills.Skills.melee))
			~get_weapon_stats:Thing.melee in
	let weapon_accuracy, weapon_damage, base_weapon_stats = effective_weapon_equip attacker attacker_equips weapon in
	let base_adjust, side_bonuses =
		if Thing.is_throwing weapon then [], []
		else
			let side_penality =
				match base_weapon_stats with
				| None -> []
				| Some ws -> [Some "not for throwing", -1 * Weapon.(ws.damage.Dice.sides / 2)] in
			[Some "not for throwing", -5], side_penality in
	let range_adjust =
		if Thing.is_throwing weapon then []
		else [Some "range", round_to_int (-.1. *. range /. 5.)] in
	let weapon_accuracy = weapon_accuracy @ (base_adjust @ range_adjust) in
	combat ~attacker ~defender ~attacker_equips ~defender_equips ~weapon:(Some weapon) ~weapon_accuracy ~weapon_damage ~side_bonuses rng
