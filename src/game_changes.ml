open Game_data
open Game_state

type stairs_dir = Up | Down

module Message =
	struct
		type t =
			| Hit of (Being.t * Being.t * int * Combat.Result.t)
			| Miss of (Being.t * Being.t * Combat.Result.t)
			| Pick_up of (Being.t * Thing.t)
			| Drop of (Being.t * Thing.t)
			| Equip of (Being.t * Equip_slot.t * Thing.t)
			| Unequip of (Being.t * Equip_slot.t * Thing.t)
			| Take_stairs of (Being.t * stairs_dir)
			| Die of Being.t
	end

module Action =
	struct
		type t =
			| Wait
			| Move of Direction.t
			| Melee_attack of Direction.t
			| Thrown_attack of (Thing.t * Map.Location.t * Being.t)
			| Pick_up of Thing.t
			| Drop of Thing.t
			| Equip of (Thing.t * Equip_slot.t)
			| Unequip of Equip_slot.t
			| Take_stairs of stairs_dir
			| Quit
	end
