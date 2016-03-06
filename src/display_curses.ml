(* TODO: handle terminal resizing *)

type screen_units = int
type screen_pos = screen_units * screen_units

type split_dir = Top | Bottom | Left | Right

type t =
	{
		mutable next_col : int;
		mutable next_col_pair : int;
		root : window;
	}
and window =
	{
		cwin : Curses.window;
		x : screen_units;
		y : screen_units;
	}
type disp = t

let init () =
	let stdscr = Curses.initscr () in
	ignore (Curses.curs_set 0);
	ignore (Curses.cbreak ());
	ignore (Curses.noecho ());
	ignore (Curses.start_color ());
	ignore (Curses.keypad stdscr true);
	{
		next_col = 0;
		next_col_pair = 1;
		root = { cwin = stdscr; x = 0; y = 0 };
	}

let close disp =
	Curses.endwin ()

let with_display f =
	let disp = init () in
	try
		f disp;
		close disp
	with e ->
		close disp;
		raise e

let root disp =
	disp.root

(* TODO: redo input handling *)
let any_key disp =
	ignore (Curses.getch ())

(* TODO: redo input handling *)
let get_key disp =
	Curses.getch ()

module Window =
	struct
		type t = window

		let dim win =
			let dimy, dimx = Curses.getmaxyx win.cwin in
			dimx, dimy

		let prepare cwin =
			Curses.scrollok cwin false

		let make disp parent (x, y) (dimx, dimy) =
			let cwin = Curses.subwin parent.cwin dimy dimx y x in
			prepare cwin;
			{
				cwin = cwin;
				x = x;
				y = y;
			}

		let split disp parent dir (dimx, dimy) =
			let par_dimx, par_dimy = dim parent in
			let dimx1, dimy1, dimx2, dimy2 =
				match dir with
				| Left | Right -> dimx, par_dimy, (par_dimx - dimx), par_dimy
				| Top | Bottom -> par_dimx, dimy, par_dimx, (par_dimy - dimy) in
			let x1, y1, x2, y2 =
				match dir with
				| Left -> 0, 0, dimx1, 0
				| Right -> dimx2, 0, 0, 0
				| Top -> 0, 0, 0, dimy1
				| Bottom -> 0, dimy2, 0, 0 in
			let cwin1 = Curses.derwin parent.cwin dimy1 dimx1 y1 x1 in
			let cwin2 = Curses.derwin parent.cwin dimy2 dimx2 y2 x2 in
			prepare cwin1;
			prepare cwin2;
			{ cwin = cwin1; x = x1; y = y1 }, { cwin = cwin2; x = x2; y = y2 }
			
		(* TODO *)
		let any_key win =
			ignore (Curses.wgetch win.cwin)
	end

module Colour =
	struct
		type t = Col of int

		type defaults =
			{
				black : t;
				red : t;
				green : t;
				yellow : t;
				blue : t;
				magenta : t;
				white : t;
			}

		let defaults =
			{
				black = Col Curses.Color.black;
				red = Col Curses.Color.red;
				green = Col Curses.Color.green;
				yellow = Col Curses.Color.yellow;
				blue = Col Curses.Color.blue;
				magenta = Col Curses.Color.magenta;
				white = Col Curses.Color.white;
			}

		let use_defaults disp =
			ignore (Curses.use_default_colors ());
			defaults

		let change disp (Col ci) (r, g, b) =
			ignore (Curses.init_color ci r g b)

		let make disp rgb =
			let c = Col disp.next_col in
			disp.next_col <- disp.next_col + 1;
			change disp c rgb;
			c
	end

module Colour_pair =
	struct
		type t = Col_pair of int

		let change disp (Col_pair cpi) (Colour.Col fgci) (Colour.Col bgci) =
			ignore (Curses.init_pair cpi fgci bgci)

		let make disp fgc bgc =
			let cp = Col_pair disp.next_col_pair in
			disp.next_col_pair <- disp.next_col_pair + 1;
			change disp cp fgc bgc;
			cp
	end

module Style =
	struct
		type t =
			{
				attrs : int;
			}

		let make ?colours ?(dim=false) ?(bright=false) disp =
			let colours_attr =
				match colours with
				| Some (Colour_pair.Col_pair cpi) ->
					Curses.A.color_pair cpi
				| None -> 0 in
			{
				attrs = 
					colours_attr
					lor (if dim then Curses.A.dim else 0)
					lor (if bright then Curses.A.bold else 0)
			}

		let default disp =
			{
				attrs = Curses.A.normal;
			}

		let apply_attrs cwin style =
			Curses.wattrset cwin style.attrs

		let apply_background style cwin ch =
			ignore (Curses.wbkgd cwin (style.attrs lor ch))
	end

module Base_view =
	struct
		type pos = screen_pos

		type t =
			{
				win : Window.t;
			}

		let make _ win =
			{
				win = win;
			}

		let dim view =
			let dimy, dimx = Curses.getmaxyx view.win.cwin in
			dimx, dimy

		let clear view =
			Curses.werase view.win.cwin

		let refresh view =
			ignore (Curses.wrefresh view.win.cwin)

		let config ?bg_style ?(bg_char=' ') view =
			begin match bg_style with
			| Some s -> Style.apply_background s view.win.cwin (int_of_char bg_char)
			| None -> ()
			end
	end

module Chars_view =
	struct
		include Base_view

		let draw view ?style (x, y) char =
			begin match style with
			| Some style -> Style.apply_attrs view.win.cwin style;
			| None -> ()
			end;
			ignore (Curses.mvwaddch view.win.cwin y x (int_of_char char))
	end

module Text_view =
	struct
		include Base_view

		let draw view ?style (x, y) str =
			begin match style with
			| Some style -> Style.apply_attrs view.win.cwin style;
			| None -> ()
			end;
			ignore (Curses.mvwaddstr view.win.cwin y x str)
	end
