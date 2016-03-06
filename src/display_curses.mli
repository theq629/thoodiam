type t
type disp = t
type screen_pos = int * int
type split_dir = Top | Bottom | Left | Right

module Window :
	sig
		type t
		val make : disp -> t -> screen_pos -> screen_pos -> t
		val split : disp -> t -> split_dir -> screen_pos -> (t * t)
		val dim : t -> screen_pos
	end

val init : unit -> t
val close : t -> unit
val with_display : (t -> unit) -> unit
val root : t -> Window.t
val any_key : t -> unit
val get_key : t -> int

module Colour :
	sig
		type t
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
		val use_defaults : disp -> defaults
		val make : disp -> (int * int * int) -> t
		val change : disp -> t -> (int * int * int) -> unit
	end

module Colour_pair :
	sig
		type t
		val make : disp -> Colour.t -> Colour.t -> t
		val change : disp -> t -> Colour.t -> Colour.t -> unit
	end

module Style :
	sig
		type t
		val make : ?colours:Colour_pair.t -> ?dim:bool -> ?bright:bool -> disp -> t
		val default : disp -> t
	end

module Chars_view :
	sig
		type t
		type pos = int * int
		val make : disp -> Window.t -> t
		val dim : t -> pos
		val clear : t -> unit
		val refresh : t -> unit
		val config : ?bg_style:Style.t -> ?bg_char:char -> t -> unit
		val draw : t -> ?style:Style.t -> pos -> char -> unit
	end

module Text_view :
	sig
		type t
		type pos = int * int
		val make : disp -> Window.t -> t
		val dim : t -> pos
		val clear : t -> unit
		val refresh : t -> unit
		val config : ?bg_style:Style.t -> ?bg_char:char -> t -> unit
		val draw : t -> ?style:Style.t -> pos -> string -> unit
	end
