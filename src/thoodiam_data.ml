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

		let main_hand = make
			~name:"main hand"
			~kind:Hand
			()

		let off_hand = make
			~name:"off hand"
			~kind:Hand
			()

		let torso = make
			~name:"torso"
			~kind:Torso
			()

		let head = make
			~name:"head"
			~kind:Head
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
				~throwing:Weapon.(make ~damage:(Dice.make 1 5) ())
				()

		let short_sword =
			make
				~tile:'/'
				~name:"short sword"
				~weight:1.5
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
				~melee:Weapon.(make ~damage:(Dice.make 3 3) ~handedness:Hand_and_a_half ())
				()

		let great_sword =
			make
				~tile:'/'
				~name:"greatsword"
				~weight:7.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 3 5) ~handedness:Two_handed ())
				()

		let spear =
			make
				~tile:'/'
				~name:"spear"
				~weight:3.
				~in_combat:In_combat.(make ~accuracy:(-1) ())
				~melee:Weapon.(make ~damage:(Dice.make 1 9) ~handedness:Hand_and_a_half ())
				~throwing:Weapon.(make ~damage:(Dice.make 1 9) ())
				()

		let great_spear =
			make
				~tile:'/'
				~name:"great spear"
				~weight:6.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 3 13) ~handedness:Two_handed ())
				()

		let glaive =
			make
				~tile:'/'
				~name:"glaive"
				~weight:7.
				~in_combat:In_combat.(make ~accuracy:(-2) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 2 9) ~handedness:Two_handed ())
				()

		let throwing_axe =
			make
				~tile:'/'
				~name:"throwing axe"
				~weight:4.5
				~melee:Weapon.(make ~damage:(Dice.make 2 4) ())
				~throwing:Weapon.(make ~damage:(Dice.make 2 4) ())
				()

		let battle_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:4.5
				~in_combat:In_combat.(make ~accuracy:(-3) ())
				~melee:Weapon.(make ~damage:(Dice.make 3 4) ~handedness:Hand_and_a_half ())
				()

		let great_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:10.
				~in_combat:In_combat.(make ~accuracy:(-4) ~evasion:1 ())
				~melee:Weapon.(make ~damage:(Dice.make 5 5) ~handedness:Two_handed ())
				()

		let quarterstaff =
			make
				~tile:'/'
				~name:"quarterstaff"
				~weight:4.
				~in_combat:In_combat.(make ~evasion:2 ())
				~melee:Weapon.(make ~damage:(Dice.make 2 3) ~handedness:Two_handed ())
				()

		let war_hammer =
			make
				~tile:'/'
				~name:"war hammer"
				~weight:5.
				~in_combat:In_combat.(make ~accuracy:(-2) ())
				~melee:Weapon.(make ~damage:(Dice.make 4 1) ~handedness:Hand_and_a_half ())
				()

		let great_hammer =
			make
				~tile:'/'
				~name:"great hammer"
				~weight:10.
				~in_combat:In_combat.(make ~accuracy:(-2) ())
				~melee:Weapon.(make ~damage:(Dice.make 4 1) ~handedness:Two_handed ())
				()

		let robe =
			make
				~tile:'['
				~name:"robe"
				~weight:1.
				~body_armour:Armour.(make ())
				()

		let leather_armour =
			make
				~tile:'['
				~name:"leather armour"
				~weight:3.
				~in_combat:In_combat.(make ~evasion:(-1) ())
				~body_armour:Armour.(make ~protection:(Dice.make 1 4) ())
				()

		let studded_leather_armour =
			make
				~tile:'['
				~name:"studded leather armour"
				~weight:5.
				~in_combat:In_combat.(make ~evasion:(-2) ())
				~body_armour:Armour.(make ~protection:(Dice.make 1 6) ())
				()

		let mail_corslet =
			make
				~tile:'['
				~name:"mail corslet"
				~weight:10.
				~in_combat:In_combat.(make ~accuracy:(-1) ~evasion:(-3) ())
				~body_armour:Armour.(make ~protection:(Dice.make 2 4) ())
				()

		let mail_hauberk =
			make
				~tile:'['
				~name:"mail hauberk"
				~weight:15.
				~in_combat:In_combat.(make ~accuracy:(-2) ~evasion:(-4) ())
				~body_armour:Armour.(make ~protection:(Dice.make 2 5) ())
				()

		let round_shield =
			make
				~tile:'['
				~name:"round shield"
				~weight:5.
				~shield:Armour.(make ~protection:(Dice.make 1 3) ())
				()

		let kite_shield =
			make
				~tile:'['
				~name:"kite shield"
				~weight:8.
				~in_combat:In_combat.(make ~accuracy:(-2) ())
				~shield:Armour.(make ~protection:(Dice.make 1 6) ())
				()

		let helm =
			make
				~tile:'['
				~name:"helm"
				~weight:2.
				~in_combat:In_combat.(make ~evasion:(-1) ())
				~helm:Armour.(make ~protection:(Dice.make 1 2) ())
				()

		let great_helm =
			make
				~tile:'['
				~name:"great helm"
				~weight:3.
				~in_combat:In_combat.(make ~evasion:(-2) ())
				~helm:Armour.(make ~protection:(Dice.make 1 3) ())
				()

		let human =
			make
				~tile:'@'
				~name:"human"
				~weight:100.
				~blocks:true
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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
				~tile:'k'
				~name:"kobold"
				~weight:70.
				~blocks:true
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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
				~equip_slots:Equip_slots.[main_hand; off_hand; torso; head]
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

		let thoodiam =
			make
				~tile:'*'
				~name:"Thoodiam"
				~weight:1.
				()
	end

module Level_spec =
	struct
		type t =
			{
				weapon_kinds : (float * Thing_kind.t) array;
				body_armour_kinds : (float * Thing_kind.t) array;
				shield_kinds : (float * Thing_kind.t) array;
				helm_kinds : (float * Thing_kind.t) array;
				enemy_kinds : (float * Thing_kind.t) array;
				unique_kinds : Thing_kind.t array;
				has_down_stairs : bool;
			}

		let make
			?(weapon_kinds=[||])
			?(body_armour_kinds=[||])
			?(shield_kinds=[||])
			?(helm_kinds=[||])
			?(enemy_kinds=[||])
			?(unique_kinds=[||])
			?(has_down_stairs=true)
			()
			=
			{ weapon_kinds; body_armour_kinds; shield_kinds; helm_kinds; enemy_kinds; has_down_stairs; unique_kinds }
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
			~body_armour_kinds:[|
					1., robe;
					1., leather_armour;
				|]
			~shield_kinds:[|
					1., round_shield;
				|]
			~helm_kinds:[|
					1., helm;
				|]
			~enemy_kinds:[|
					1., goblin;
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
				|]
			~body_armour_kinds:[|
					1., leather_armour;
					1., studded_leather_armour;
				|]
			~shield_kinds:[|
					1., round_shield;
				|]
			~helm_kinds:[|
					1., helm;
				|]
			~enemy_kinds:[|
					1., goblin;
					1., kobold;
				|]
			()
		);
		Level_spec.(make
			~weapon_kinds:[|
					1., dagger;
					1., short_sword;
					1., quarterstaff;
					2., long_sword;
					2., spear;
					2., battle_axe;
					2., war_hammer;
					2., bastard_sword;
				|]
			~body_armour_kinds:[|
					1., leather_armour;
					2., studded_leather_armour;
					2., mail_corslet;
				|]
			~shield_kinds:[|
					1., round_shield;
					1., kite_shield;
				|]
			~helm_kinds:[|
					1., helm;
					1., great_helm;
				|]
			~enemy_kinds:[|
					1., goblin;
					1., kobold;
					2., orc;
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
					2., bastard_sword;
					1., great_sword;
					2., great_axe;
					2., great_spear;
					2., great_hammer;
				|]
			~body_armour_kinds:[|
					1., leather_armour;
					1., studded_leather_armour;
					2., mail_corslet;
					2., mail_hauberk;
				|]
			~shield_kinds:[|
					1., round_shield;
					2., kite_shield;
				|]
			~helm_kinds:[|
					1., helm;
					2., great_helm;
				|]
			~enemy_kinds:[|
					1., goblin;
					1., kobold;
					2., orc;
					2., ogre;
				|]
			()
		);
		Level_spec.(make
			~has_down_stairs:false
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
			~body_armour_kinds:[|
					1., leather_armour;
					1., studded_leather_armour;
					1., mail_corslet;
					1., mail_hauberk;
				|]
			~shield_kinds:[|
					1., round_shield;
					1., kite_shield;
				|]
			~helm_kinds:[|
					1., helm;
					1., great_helm;
				|]
			~enemy_kinds:[|
					1., goblin;
					1., kobold;
					1., orc;
					2., ogre;
					2., giant;
				|]
			~unique_kinds:[|
					thoodiam
				|]
			()
		);
	|]

let welcome_text =
	Printf.sprintf
		"You have heard that the artifact known only as the Thoodiam lies on the %s level of this dungeon. You must reach it and bring it back to the surface to succeed."
		(English.int (Array.length level_specs))

let game_over_text =
	function
	| Game.Playing -> "You are still playing!"
	| Game.Lost Game.Died -> "You died."
	| Game.Lost Game.Left -> "You lost by leaving the dungeon without the Thoodiam."
	| Game.Won -> "You won!"
