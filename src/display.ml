module type I =
	sig
		type t
		type disp = t
		type screen_pos = int * int

		module Window :
			sig
				type t
				val make : disp -> t -> screen_pos -> screen_pos -> t
				val dim : t -> screen_pos
			end

		val root : t -> Window.t

		module Style :
			sig
				type t
			end

		module Chars_view :
			sig
				type t
				type pos = int * int
				val make : disp -> Window.t -> t
				val dim : t -> pos
				val clear : t -> unit
				val refresh : t -> unit
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
				val draw : t -> ?style:Style.t -> pos -> string -> unit
			end
	end
