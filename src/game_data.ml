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

		let zero =
			{ num = 0; sides = 0 }

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

module In_combat =
	struct
		type t =
			{
				accuracy : int;
				evasion : int;
			}

		let make
			?(accuracy=0)
			?(evasion=0)
			()
			=
			{ accuracy; evasion }
	end

module Weapon =
	struct
		type handedness = One_handed | Hand_and_a_half | Two_handed

		type t =
			{
				damage : Dice.t;
				handedness : handedness;
			}

		let make
			?(damage=Dice.zero)
			?(handedness=One_handed)
			()
			=
			{ damage; handedness }
	end

module Armour =
	struct
		type t =
			{
				protection : Dice.t;
			}

		let make
			?(protection=Dice.zero)
			()
			=
			{ protection }
	end

module Skills =
	struct
		type t =
			{
				melee : int;
				evasion : int;
			}

		let default =
			{
				melee = 0;
				evasion = 0;
			}

		let make
			?(melee=0)
			?(evasion=0)
			()
			=
			{ melee; evasion }
	end

module Bodyable =
	struct
		type t =
			{
				vision : int;
				str : int;
				dex : int;
				con : int;
				def_skills : Skills.t;
			}

		let make
			?(vision=10)
			?(str=0)
			?(dex=0)
			?(con=0)
			?(def_skills=Skills.default)
			()
			=
			{ vision; str; dex; con; def_skills }
	end

module Equip_slot =
	struct
		type kind = Hand | Torso | Head

		type t =
			{
				name : string;
				kind : kind;
			}

		let make
			~name
			~kind
			()
			=
			{ name; kind }
	end

module Thing_kind =
	struct
		type t =
			{
				tile : tile;
				name : string;
				weight : float;
				in_combat : In_combat.t option;
				melee : Weapon.t option;
				shield : Armour.t option;
				body_armour : Armour.t option;
				helm : Armour.t option;
				blocks : bool;
				equip_slots : Equip_slot.t list;
				bodyable : Bodyable.t option;
			}

		let make
			~tile
			~name
			~weight
			?in_combat
			?melee
			?shield
			?body_armour
			?helm
			?(blocks=false)
			?(equip_slots=[])
			?bodyable
			()
			=
			{ tile; name; weight; in_combat; melee; shield; body_armour; helm; blocks; equip_slots; bodyable }
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
