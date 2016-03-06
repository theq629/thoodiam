(*
 * Adam Milazzo's FOV algorithm.
 * http://www.adammil.net/blog/v125_Roguelike_Vision_Algorithms.html
 *)

exception End_sector
exception End_octant

module Slope =
	struct
		type t = int * int
		let ( > ) (x1, y1) (x2, y2) = y1 * x2 > x1 * y2
		let ( >= ) (x1, y1) (x2, y2) = y1 * x2 >= x1 * y2
		let ( < ) (x1, y1) (x2, y2) = y1 * x2 < x1 * y2
	end

let dist (x1, y1) (x2, y2) =
	let dx = float_of_int (x2 - x1) in
	let dy = float_of_int (y2 - y1) in
	int_of_float (0.5 +. sqrt (dx**2. +. dy**2.))

let octantize octant (ox, oy) f =
	match octant with
	| 0 -> (fun (x, y) -> f (ox + x, oy - y))
	| 1 -> (fun (x, y) -> f (ox + y, oy - x))
	| 2 -> (fun (x, y) -> f (ox - y, oy - x))
	| 3 -> (fun (x, y) -> f (ox - x, oy - y))
	| 4 -> (fun (x, y) -> f (ox - x, oy + y))
	| 5 -> (fun (x, y) -> f (ox - y, oy + x))
	| 6 -> (fun (x, y) -> f (ox + y, oy + x))
	| 7 -> (fun (x, y) -> f (ox + x, oy + y))
	| _ -> assert false

let rec compute_octant blocks set_visible origin_dist range start_x init_top init_bot =
	let calc_y x (s_x, s_y) =
		((x * 2 - 1) * s_y + s_x) / (2 * s_x) in

	let top = ref init_top in
	let bot = ref init_bot in

	for x = start_x to range do
		let upper_y =
			if let top_x, _ = !top in top_x == 1 then begin
				x
			end else begin
				let y = calc_y x !top in
				if blocks (x, y) then begin
					if Slope.(!top >= (2 * x, 2 * y + 1))
						&& not (blocks (x, y + 1)) then
						y + 1
					else
						y
				end else begin
					let adjust_x =
						if blocks (x + 1, y + 1) then 1
						else 0 in
					if Slope.(!top > (x * 2 + adjust_x, y * 2 + 1)) then
						y + 1
					else
						y
				end
			end in

		let lower_y =
			if let _, bot_y = !bot in bot_y == 0 then begin
				0
			end else begin
				let y = calc_y x !bot in
				if Slope.(!bot >= (2 * x, 2 * y + 1))
					&& blocks (x, y)
					&& not (blocks (x, y + 1)) then
					y + 1
				else
					y
			end in

		let was_opaque = ref (-1) in
		begin try
			for y = upper_y downto lower_y do
				if origin_dist (x, y) <= range then begin
					let is_opaque = blocks (x, y) in
					let is_visible =
						is_opaque || (
							(y != upper_y || Slope.(!top > (x * 4 + 1, y * 4 - 1)))
							&& (y != lower_y || Slope.(!bot < (x * 4 - 1, y * 4 + 1)))
						) in

					if is_visible then
						set_visible (x, y);
					
					if x != range then begin
						if is_opaque then begin
							if !was_opaque == 0 then begin
								let new_bot =
									x * 2 - (if blocks (x, y + 1) then 1 else 0),
									y * 2 + 1 in
								if Slope.(!top > new_bot) then begin
									if y == lower_y then begin
										bot := new_bot;
										raise End_sector
									end else
										compute_octant blocks set_visible origin_dist range (x + 1) !top new_bot
								end else begin
									if y == lower_y then
										raise End_octant
								end
							end;
							was_opaque := 1
						end else begin
							if !was_opaque == 1 then begin
								let new_top_x, new_top_y as new_top =
									x * 2 + (if blocks (x + 1, y + 1) then 1 else 0),
									y * 2 + 1 in
								if Slope.(!bot >= new_top) then
									raise End_octant;
								top := new_top
							end;
							was_opaque := 0
						end
					end
				end
			done
		with End_sector -> ()
		end;

		if !was_opaque != 0 then
			raise End_octant
	done

let compute blocks set_visible origin range =
	for octant = 0 to 8 - 1 do
		let oct_blocks = octantize octant origin blocks in
		let oct_set_vis = octantize octant origin set_visible in
		let oct_origin_dist = octantize octant origin (dist origin) in
		try
			compute_octant oct_blocks oct_set_vis oct_origin_dist range 1 (1, 1) (1, 0)
		with End_octant -> ()
	done
