open Std
open Game_data

let melee_combat region attacker defender rng =
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
		Opt.flat_map begin fun a_wpn ->
			Opt.map begin fun a_wpn_cbt ->
				let d_amrs =
					let amr_slots = List.filter (fun es -> es.Equip_slot.is_armour) attacker.Being.body.Thing.kind.Thing.Kind.equip_slots in
					List.filter_map (in_slot attacker) amr_slots in
				let a_extra_dice = (attack_roll - evasion_roll) / (7 + round_to_int a_wpn.Thing.kind.Thing.Kind.weight) in
				let damage_roll = Dice.roll Game_data.Combat.(Dice.(make (a_wpn_cbt.damage.num + a_extra_dice) (a_wpn_cbt.damage.sides + max (round_to_int a_wpn.Thing.kind.Thing.Kind.weight) a_str))) rng in
			ignore (a_str, a_wpn, d_amrs);
				let protection_roll =
					List.fold_left begin fun r a ->
						r + match a.Thing.kind.Thing.Kind.armour with
							| Some a -> Dice.roll a.Game_data.Combat.protection rng
							| None -> 0
					end 0 d_amrs in
				max 0 (damage_roll - protection_roll)
			end a_wpn.Thing.kind.Thing.Kind.melee
		end a_wpn
	end else
		None
