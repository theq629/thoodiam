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
	end

module Equip_slots =
	struct
		open Equip_slot

		let melee_weapon = make
			~name:"weapon"
			~is_melee:true
			()

		let armour = make
			~name:"armour"
			~is_armour:true
			()
	end

module Thing_kinds =
	struct
		open Thing.Kind

		let dagger =
			make
				~tile:'/'
				~name:"dagger"
				~weight:0.5
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 1 5)
					())
				()

		let short_sword =
			make
				~tile:'/'
				~name:"short sword"
				~weight:2.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 0 7)
						~evasion:1
					())
				()

		let long_sword =
			make
				~tile:'/'
				~name:"longsword"
				~weight:3.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 2 5)
						~evasion:1
					())
				()

		let bastard_sword =
			make
				~tile:'/'
				~name:"bastard sword"
				~weight:4.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 3 3)
						~evasion:1
					())
				()

		let great_sword =
			make
				~tile:'/'
				~name:"greatsword"
				~weight:5.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 3 5)
						~evasion:1
					())
				()

		let spear =
			make
				~tile:'/'
				~name:"spear"
				~weight:2.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 1 9)
						~evasion:0
					())
				()

		let great_spear =
			make
				~tile:'/'
				~name:"great spear"
				~weight:3.
				~melee:Combat.(make
						~accuracy:(-1)
						~damage:(Dice.make 1 13)
						~evasion:1
					())
				()

		let glaive =
			make
				~tile:'/'
				~name:"glaive"
				~weight:4.
				~melee:Combat.(make
						~accuracy:(-2)
						~damage:(Dice.make 2 9)
						~evasion:1
					())
				()

		let battle_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:4.
				~melee:Combat.(make
						~accuracy:(-3)
						~damage:(Dice.make 3 4)
						~evasion:0
					())
				()

		let great_axe =
			make
				~tile:'/'
				~name:"battle axe"
				~weight:5.
				~melee:Combat.(make
						~accuracy:(-4)
						~damage:(Dice.make 4 4)
						~evasion:0
					())
				()

		let quarterstaff =
			make
				~tile:'/'
				~name:"quarterstaff"
				~weight:1.
				~melee:Combat.(make
						~accuracy:0
						~damage:(Dice.make 2 3)
						~evasion:2
					())
				()

		let war_hammer =
			make
				~tile:'/'
				~name:"war hammer"
				~weight:5.
				~melee:Combat.(make
						~accuracy:(-2)
						~damage:(Dice.make 4 1)
						~evasion:0
					())
				()

		let robe =
			make
				~tile:'['
				~name:"robe"
				~weight:1.
				~armour:Combat.(make ())
				()

		let leather_armour =
			make
				~tile:'['
				~name:"leather armour"
				~weight:3.
				~armour:Combat.(make
						~evasion:(-1)
						~protection:(Dice.make 1 4)
					())
				()

		let studded_leather_armour =
			make
				~tile:'['
				~name:"studded leather armour"
				~weight:5.
				~armour:Combat.(make
						~evasion:(-2)
						~protection:(Dice.make 1 6)
					())
				()

		let mail_corslet =
			make
				~tile:'['
				~name:"mail corslet"
				~weight:10.
				~armour:Combat.(make
						~accuracy:(-1)
						~evasion:(-3)
						~protection:(Dice.make 2 4)
					())
				()

		let mail_hauberk =
			make
				~tile:'['
				~name:"mail hauberk"
				~weight:15.
				~armour:Combat.(make
						~accuracy:(-2)
						~evasion:(-4)
						~protection:(Dice.make 2 5)
					())
				()

		let human =
			make
				~tile:'@'
				~name:"human"
				~weight:100.
				~visual_priority:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:10
						~str:7
						~dex:6
						~con:10
					())
				()

		let goblin =
			make
				~tile:'g'
				~name:"goblin"
				~weight:70.
				~visual_priority:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:8
						~str:2
						~dex:3
						~con:2
					())
				()

		let orc =
			make
				~tile:'o'
				~name:"orc"
				~weight:100.
				~visual_priority:true
				~equip_slots:Equip_slots.[melee_weapon; armour]
				~bodyable:Bodyable.(make
						~vision:6
						~str:3
						~dex:2
						~con:4
					())
				()
	end
