open Std
open Game_data
module Map_search = Graph_search.Make(Tilemap.Location)

module Thing =
	struct
		open Thing_kind

		type t =
			{
				kind : Thing_kind.t;
			}

		let make kind =
			{ kind }

		let tile thing =
			thing.kind.tile

		let name thing =
			thing.kind.name

		let weight thing =
			thing.kind.weight

		let in_combat thing =
			thing.kind.in_combat

		let melee thing =
			thing.kind.melee

		let armour thing =
			thing.kind.armour

		let blocks thing =
			thing.kind.blocks

		let equip_slots thing =
			thing.kind.equip_slots

		let bodyable thing =
			thing.kind.bodyable
	end

module Being =
	struct
		type t =
			{
				body : Thing.t;
				skills : Skills.t;
				mutable at : Map.Location.t;
				mutable inv : Thing.t list;
				mutable equip : (Equip_slot.t * Thing.t) list;
				mutable can_carry : float;
				mutable inv_weight : float;
				mutable max_hp : int;
				mutable hp : int;
				mutable stress : int;
			}

		let make body_kind at =
			let body = Thing.make body_kind in
			let scale_stat base scale = 0.5 +. base *. 1.2**(float_of_int scale) in
			let max_hp, can_carry, skills =
				match Thing.(bodyable body) with
				| None -> 0, 0., Skills.default
				| Some b ->
					Bodyable.(
						round_to_int (scale_stat 2. b.con),
						scale_stat 4. b.str,
						b.def_skills
					) in
			{
				at = at;
				skills = skills;
				body = body;
				inv = [];
				equip = [];
				can_carry = can_carry;
				inv_weight = 0.;
				max_hp = max_hp;
				hp = max_hp;
				stress = 0;
			}

		let body being =
			being.body

		let inv being =
			being.inv

		let equip_slots being =
			being.body.Thing.kind.Thing_kind.equip_slots

		let in_slot being es =
			try Some (List.assoc es (being.equip))
			with Not_found -> None

		let get being thing =
			let new_weight = being.inv_weight +. thing.Thing.kind.Thing_kind.weight in
			if new_weight <= being.can_carry then begin
				being.inv <- thing::being.inv;
				being.inv_weight <- being.inv_weight +. thing.Thing.kind.Thing_kind.weight;
				true
			end else
				false

		let lose being thing =
			let found, inv1 = remove_from_list being.inv thing in
			if found then begin
				being.inv <- inv1;
				being.inv_weight <- being.inv_weight -. thing.Thing.kind.Thing_kind.weight;
				true
			end else
				false

		let equip being equip_slot thing =
			if List.mem equip_slot being.body.Thing.kind.Thing_kind.equip_slots && not (List.mem_assoc equip_slot being.equip) then begin
				let found, inv1 = remove_from_list being.inv thing in
				if found then begin
					being.inv <- inv1;
					being.equip <- (equip_slot, thing)::being.equip;
					true
				end else
					false
			end else
				false

		let unequip being equip_slot =
			try
				let thing = List.assoc equip_slot being.equip in
				being.equip <- List.remove_assoc equip_slot being.equip;
				being.inv <- thing::being.inv;
				Some thing
			with Not_found ->
				None
	end
