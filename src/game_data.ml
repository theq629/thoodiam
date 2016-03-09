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
			}

		let make
			~name
			()
			=
			{ name }
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
		type t =
			{
				body : Thing.t;
				mutable at : Map.Location.t;
				mutable inv : Thing.t list;
				mutable equip : (Equip_slot.t * Thing.t) list;
				mutable can_carry : float;
				mutable inv_weight : float;
				mutable max_hp : int;
				mutable hp : int;
			}
	end
