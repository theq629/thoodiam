module Rng = Variates.Make(Variates.Stdlib_source)
module Vec = Vectors.TwoD
module Map = Tilemap.Square
module Fov = Fov_adammil

type tile = char

module Dice =
	struct
		type t =
			{
				num : int;
				sides : int;
			}

		let make num sides =
			{ num; sides }

		let to_string dice =
			Printf.sprintf "%id%i" dice.num dice.sides

		let roll dice rng =
			let rec run sum i =
				if i <= 0 then sum
				else
					let roll = 1 + Rng.Uniform.int 0 dice.sides rng in
					run (sum + roll) (i - 1) in
			run 0 dice.num

		let is_zero d =
			d.num == 0 || d.sides == 0
	end

module Combat =
	struct
		type t =
			{
				accuracy : int;
				damage : Dice.t;
				evasion : int;
				protection : Dice.t;
			}

		let make
			?(accuracy=0)
			?(damage=Dice.make 0 0)
			?(evasion=0)
			?(protection=(Dice.make 0 0))
			()
			=
			{ accuracy; damage; evasion; protection }
	end

module Bodyable =
	struct
		type t =
			{
				vision : int;
				str : int;
				dex : int;
				con : int;
			}

		let make
			?(vision=10)
			?(str=0)
			?(dex=0)
			?(con=0)
			()
			=
			{ vision; str; dex; con }
	end

module Equip_slot =
	struct
		type t =
			{
				name : string;
				is_melee : bool;
				is_armour : bool;
			}

		let make
			~name
			?(is_melee=false)
			?(is_armour=false)
			()
			=
			{ name; is_melee; is_armour }
	end

module Thing =
	struct
		module Kind =
			struct
				type t =
					{
						tile : tile;
						name : string;
						weight : float;
						melee : Combat.t option;
						armour : Combat.t option;
						visual_priority : bool;
						equip_slots : Equip_slot.t list;
						bodyable : Bodyable.t option;
					}

				let make
					~tile
					~name
					~weight
					?melee
					?armour
					?(visual_priority=false)
					?(equip_slots=[])
					?bodyable
					()
					=
					{ tile; name; weight; melee; armour; visual_priority; equip_slots; bodyable }
			end

		type t =
			{
				kind : Kind.t;
			}

		let make kind =
			{ kind }
	end

module Terrain =
	struct
		type t =
			{
				tile : tile;
				blocking : bool;
			}

		let make
			~tile
			?(blocking=false)
			()
			=
			{ tile; blocking }
	end

module Being =
	struct
		type skills =
			{
				melee : int;
				evasion : int;
			}

		type t =
			{
				body : Thing.t;
				skills : skills;
				mutable at : Map.Location.t;
				mutable inv : Thing.t list;
				mutable equip : (Equip_slot.t * Thing.t) list;
				mutable can_carry : float;
				mutable inv_weight : float;
				mutable max_hp : int;
				mutable hp : int;
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

module Direction =
	struct
		type t = N | S | E | W | NE | NW | SE | SW

		let to_vec = function
			| N -> (0, -1)
			| S -> (0, 1)
			| E -> (1, 0)
			| W -> (-1, 0)
			| NE -> (1, -1)
			| NW -> (-1, -1)
			| SE -> (1, 1)
			| SW -> (-1, 1)

		let of_vec = function
			| (0, -1) -> Some N
			| (0, 1) -> Some S
			| (1, 0) -> Some E
			| (-1, 0) -> Some W
			| (1, -1) -> Some NE
			| (-1, -1) -> Some NW
			| (1, 1) -> Some SE
			| (-1, 1) -> Some SW
			| _ -> None
	end

module Action =
	struct
		type t =
			| Move of Direction.t
			| Melee_attack of Direction.t
			| Pick_up of Thing.t
			| Drop of Thing.t
			| Equip of (Thing.t * Equip_slot.t)
			| Unequip of Equip_slot.t
			| Quit
	end

let in_slot being es =
	try Some (List.assoc es (being.Being.equip))
	with Not_found -> None
