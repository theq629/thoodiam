open Game_data

module Message =
	struct
		type t =
			| Melee_hit of (Being.t * Being.t * int * Combat_system.Result.t)
			| Melee_miss of (Being.t * Being.t * Combat_system.Result.t)
			| Pick_up of (Being.t * Thing.t)
			| Drop of (Being.t * Thing.t)
			| Equip of (Being.t * Equip_slot.t * Thing.t)
			| Unequip of (Being.t * Equip_slot.t * Thing.t)
			| Die of Being.t
	end

module Action =
	struct
		type t =
			| Wait
			| Move of Direction.t
			| Melee_attack of Direction.t
			| Pick_up of Thing.t
			| Drop of Thing.t
			| Equip of (Thing.t * Equip_slot.t)
			| Unequip of Equip_slot.t
			| Quit
	end
