type t
type disp = t
type screen_pos = int * int
type split_dir = Horiz | Vert
type key = int

module Window :
	sig
		type t
		val make : disp -> t -> screen_pos -> screen_pos -> t
		val split : disp -> t -> split_dir -> float -> (t * t)
		val dim : t -> screen_pos
	end

val init : Dom_html.element Js.t -> t
val close : t -> unit
val root : t -> Window.t
val input_loop : t -> (key option -> bool) -> unit

module Colour :
	sig
		type t
		val of_string : string -> t
	end

module Style :
	sig
		type t
		val make : ?fg:Colour.t -> ?bg:Colour.t -> disp -> t
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
		val config : ?font_pt:int -> ?font_name:string -> ?fg:Colour.t -> ?bg:Colour.t -> t -> unit
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
		val config : ?font_pt:int -> ?font_name:string -> ?fg:Colour.t -> ?bg:Colour.t -> t -> unit
		val draw : t -> ?style:Style.t -> pos -> string -> unit
	end
