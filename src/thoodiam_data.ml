open Game_data
open Game_data

module Terrains =
	struct
		let floor = Terrain.make
			~tile:'.'
			()

		let wall = Terrain.make
			~tile:'#'
			~blocking:true
			()

		let stairs_up = Terrain.make
			~tile:'<'
			()

		let stairs_down = Terrain.make
			~tile:'>'
			()
	end

module Equip_slots =
	struct
		open Equip_slot

		let melee_weapon = make
			~name:"weapon"
			~is_melee:true
			~affects_combat:true
			()

		let armour = make
			~name:"armour"
			~is_armour:true
			~affects_combat:true
			()
	end

module Thing_kinds =
	struct
		open Thing_kind

		let dagger =
			make
				~tile:'/'
				~name:"dagger"
				~weight:0.5
				~melee:Weapon.(make ~damage:(Dice.make 1 5) ())
				()

		let short_sword =
			make
				~tile:'/'
				~name:"short sword"
				~weight:2.
				~in_combat:In_combat.(make ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 1 7) ())
				()

		let long_sword =
			make
				~tile:'/'
				~name:"longsword"
				~weight:3.
				~in_combat:In_combat.(make ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 2 5) ())
				()

		let bastard_sword =
			make
				~tile:'/'
				~name:"bastard sword"
				~weight:4.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 3 3) ())
				()

		let great_sword =
			make
				~tile:'/'
				~name:"greatsword"
				~weight:5.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 3 5) ())
				()

		let spear =
			make
				~tile:'/'
				~name:"spear"
				~weight:2.
				~in_combat:In_combat.(make ~accuracy:(-1) ())
				~melee:Weapon.(make ~damage:(Dice.make 1 9) ())
				()

		let great_spear =
			make
				~tile:'/'
				~name:"great spear"
				~weight:3.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 3 13) ())
				()

		let glaive =
			make
				~tile:'/'
				~name:"glaive"
				~weight:4.
				~in_combat:In_combat.(make ~accuracy:(-2) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 2 9) ())
				()

		let battle_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:4.
				~in_combat:In_combat.(make ~accuracy:(-3) ())
				~melee:Weapon.(make ~damage:(Dice.make 3 4) ())
				()

		let great_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:5.
				~in_combat:In_combat.(make ~accuracy:(-4) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 5 5) ())
				()

		let quarterstaff =
			make
				~tile:'/'
				~name:"quarterstaff"
				~weight:1.
				~in_combat:In_combat.(make ~evasion:2 ())
				~melee:Weapon.(make ~damage:(Dice.make 2 3) ())
				()

		let war_hammer =
			make
				~tile:'/'
				~name:"war hammer"
				~weight:5.
				~in_combat:In_combat.(make ~accuracy:(-2) ())
				~melee:Weapon.(make ~damage:(Dice.make 4 1) ())
				()

		let great_hammer =
			make
				~tile:'/'
				~name:"great hammer"
				~weight:5.
				~in_combat:In_combat.(make ~accuracy:(-2) ())
				~melee:Weapon.(make ~damage:(Dice.make 4 1) ())
				()

		let robe =
			make
				~tile:'['
				~name:"robe"
				~weight:1.
				~armour:Armour.(make ())
				()

		let leather_armour =
			make
				~tile:'['
				~name:"leather armour"
				~weight:3.
				~in_combat:In_combat.(make ~evasion:(-1) ())
				~armour:Armour.(make ~protection:(Dice.make 1 4) ())
				()

		let studded_leather_armour =
			make
				~tile:'['
				~name:"studded leather armour"
				~weight:5.
				~in_combat:In_combat.(make ~evasion:(-2) ())
				~armour:Armour.(make ~protection:(Dice.make 1 6) ())
				()

		let mail_corslet =
			make
				~tile:'['
				~name:"mail corslet"
				~weight:10.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:(-3) ())
				~armour:Armour.(make ~protection:(Dice.make 2 4) ())
				()

		let mail_hauberk =
			make
				~tile:'['
				~name:"mail hauberk"
				~weight:15.
				~in_combat:In_combat.(make ~accuracy:(-2) ~evasion:(-4) ())
				~armour:Armour.(make ~protection:(Dice.make 2 5) ())
				()

		let human =
			make
				~tile:'@'
				~name:"human"
				~weight:100.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:10
						~str:7
						~dex:6
						~con:10
						~def_skills:Skills.(make
							~melee:10
							~evasion:7
						())
					())
				()

		let kobold =
			make
				~tile:'g'
				~name:"goblin"
				~weight:70.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:8
						~str:2
						~dex:4
						~con:2
						~def_skills:Skills.(make
							~melee:6
							~evasion:4
						())
					())
				()

		let goblin =
			make
				~tile:'g'
				~name:"goblin"
				~weight:70.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:6
						~str:2
						~dex:2
						~con:2
						~def_skills:Skills.(make
							~melee:4
							~evasion:4
						())
					())
				()

		let orc =
			make
				~tile:'o'
				~name:"orc"
				~weight:100.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:6
						~str:4
						~dex:3
						~con:4
						~def_skills:Skills.(make
							~melee:6
							~evasion:4
						())
					())
				()

		let ogre =
			make
				~tile:'O'
				~name:"orge"
				~weight:150.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:8
						~str:6
						~dex:3
						~con:6
						~def_skills:Skills.(make
							~melee:6
							~evasion:4
						())
					())
				()

		let giant =
			make
				~tile:'H'
				~name:"giant"
				~weight:200.
				~blocks:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:10
						~str:8
						~dex:4
						~con:8
						~def_skills:Skills.(make
							~melee:8
							~evasion:4
						())
					())
				()
	end

module Level_spec =
	struct
		type t =
			{
				weapon_kinds : (float * Thing_kind.t) array;
				armour_kinds : (float * Thing_kind.t) array;
				enemy_kinds : (float * Thing_kind.t) array;
			}

		let make
			?(weapon_kinds=[||])
			?(armour_kinds=[||])
			?(enemy_kinds=[||])
			()
			=
			{ weapon_kinds; armour_kinds; enemy_kinds }
	end

let level_specs =
	let open Thing_kinds in
	[|
		Level_spec.(make
			~weapon_kinds:[|
					2., dagger;
					1., short_sword;
					1., quarterstaff;
				|]
			~armour_kinds:[|
					1., leather_armour;
				|]
			~enemy_kinds:[|
					1., goblin;
				|]
			()
		);
		Level_spec.(make
			~weapon_kinds:[|
					2., dagger;
					2., short_sword;
					2., quarterstaff;
					1., long_sword;
					1., spear;
					1., battle_axe;
					1., war_hammer;
				|]
			~armour_kinds:[|
					1., robe;
					2., leather_armour;
					1., studded_leather_armour;
				|]
			~enemy_kinds:[|
					1., goblin;
					1., kobold;
				|]
			()
		);
		Level_spec.(make
			~weapon_kinds:[|
					2., dagger;
					2., short_sword;
					2., quarterstaff;
					2., long_sword;
					2., spear;
					2., battle_axe;
					2., war_hammer;
					1., bastard_sword;
				|]
			~armour_kinds:[|
					2., leather_armour;
					2., studded_leather_armour;
					1., mail_corslet;
				|]
			~enemy_kinds:[|
					2., goblin;
					2., kobold;
					1., orc;
				|]
			()
		);
		Level_spec.(make
			~weapon_kinds:[|
					2., dagger;
					2., short_sword;
					2., quarterstaff;
					2., long_sword;
					2., spear;
					2., battle_axe;
					2., war_hammer;
					2., bastard_sword;
					1., great_sword;
					1., great_axe;
					1., great_spear;
					1., great_hammer;
				|]
			~armour_kinds:[|
					2., leather_armour;
					2., studded_leather_armour;
					2., mail_corslet;
					1., mail_hauberk;
				|]
			~enemy_kinds:[|
					2., goblin;
					2., kobold;
					2., orc;
					1., ogre;
				|]
			()
		);
		Level_spec.(make
			~weapon_kinds:[|
					1., dagger;
					1., short_sword;
					1., quarterstaff;
					1., long_sword;
					1., spear;
					1., battle_axe;
					1., war_hammer;
					1., bastard_sword;
					1., great_sword;
					1., great_axe;
					1., great_spear;
					1., great_hammer;
				|]
			~armour_kinds:[|
					1., leather_armour;
					1., studded_leather_armour;
					1., mail_corslet;
					1., mail_hauberk;
				|]
			~enemy_kinds:[|
					2., goblin;
					2., kobold;
					2., orc;
					2., ogre;
					1., giant;
				|]
			()
		);
	|]
