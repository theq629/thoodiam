type split_dir = Horiz | Vert

module Abstract_window =
	struct
		type screen_pos = int * int

		type t =
			{
				mutable pos : screen_pos;
				mutable dim : screen_pos;
				mutable subs : sub list;
				mutable view_layouts : (t -> unit) list;
			}
		and sub = Float of t | Split of (split_dir * float * t * t)

		let dim view =
			view.dim

		let rec layout win =
			List.iter begin fun view_layout ->
				view_layout win
			end win.view_layouts;
			List.iter begin fun sub ->
				match sub with
				| Float child ->
					layout child;
				| Split (dir, frac, child1, child2) ->
					begin match dir with
					| Horiz ->
						let w = let dx, _ = win.dim in int_of_float (frac *. float_of_int dx) in
						child1.pos <- win.pos;
						child1.dim <- (let _, dy = win.dim in (w, dy));
						child2.pos <- (let x, y = win.pos in (x + w, y));
						child2.dim <- (let dx, dy = win.dim in (dx - w, dy))
					| Vert ->
						let w = let _, dy = win.dim in int_of_float (frac *. float_of_int dy) in
						child1.pos <- win.pos;
						child1.dim <- (let dx, _ = win.dim in (dx, w));
						child2.pos <- (let x, y = win.pos in (x, y + w));
						child2.dim <- (let dx, dy = win.dim in (dx, dy - w))
					end;
					layout child1;
					layout child2
			end win.subs

		let make disp parent pos dim =
			{
				pos = pos;
				dim = dim;
				subs = [];
				view_layouts = [];
			}

		let make_dummy () =
			{
				pos = (0, 0);
				dim = (0, 0);
				subs = [];
				view_layouts = [];
			}

		let split disp parent split_dir frac =
			let child1 = make_dummy () in
			let child2 = make_dummy () in
			parent.subs <- (Split (split_dir, frac, child1, child2))::parent.subs;
			layout parent;
			child1, child2
	end

module Watcher_set =
	struct
		type watcher = Watcher of int

		type 'a t =
			{
				mutable next_id : int;
				mutable all : (int * ('a -> unit)) list;
			}

		let make () =
			{
				next_id = 0;
				all = [];
			}

		let add ws f =
			let wi = ws.next_id in
			ws.all <- (wi, f) :: ws.all;
			ws.next_id <- ws.next_id + 1;
			Watcher wi

		let remove ws (Watcher wi) =
			ws.all <- List.remove_assoc wi ws.all

		let trigger ws x =
			List.iter (fun (_, f) -> f x) ws.all
	end

module Window = Abstract_window

type screen_pos = Abstract_window.screen_pos
type key = int

type t =
	{
		container_elt : Dom_html.element Js.t;
		root : Window.t;
		mutable view_elts : Dom.element Js.t list;
		refresh_watchers : t Watcher_set.t;
	}
type disp = t

let debounce timeout callback =
	let timer = ref None in
	fun _ ->
		begin match !timer with
		| Some timer_value ->
			Dom_html.window##clearTimeout (timer_value);
		| None -> ()
		end;
		timer := Some (Dom_html.window##setTimeout (callback, timeout));
		Js._true

let handle_resize ui () =
	Window.(
		ui.root.pos <- (ui.container_elt##clientLeft, ui.container_elt##clientTop);
		ui.root.dim <- (ui.container_elt##clientWidth, ui.container_elt##clientHeight);
		layout ui.root
	);
	Watcher_set.trigger ui.refresh_watchers ui

let remove_view_elt ui view_elt =
	Dom.removeChild ui.container_elt view_elt

let init container_elt =
	let ui =
		{
			container_elt = container_elt;
			root = Window.make_dummy ();
			view_elts = [];
			refresh_watchers = Watcher_set.make ();
		} in
	handle_resize ui ();
	Dom_html.window##onresize <- Dom_html.handler (debounce 100. (Js.wrap_callback (handle_resize ui)));
	ui

let close ui =
	List.iter begin fun view_elt ->
		remove_view_elt ui view_elt
	end ui.view_elts

let root ui =
	ui.root

let input_loop ui f =
	let key_watcher = ref None in
	let refresh_watcher = ref None in

	let finish () =
		begin match !key_watcher with
		| Some w -> Dom_html.removeEventListener w
		| None -> ()
		end;
		begin match !refresh_watcher with
		| Some w -> Watcher_set.remove ui.refresh_watchers w
		| None -> ()
		end in

	let on_refresh _ =
		if not (f None) then
			finish () in
	let on_key event =
		if not (f (Some event##keyCode)) then
			finish ();
		Js._true in

	if f None then begin
		key_watcher := Some (Dom_html.addEventListener Dom_html.document Dom_html.Event.keydown (Dom_html.handler on_key) Js._false);
		refresh_watcher := Some (Watcher_set.add ui.refresh_watchers on_refresh)
	end

let get_key ui =
	(* TODO: really need a better way to handle input to avoid busy waiting *)
	let got_key = ref None in
	input_loop ui begin fun key ->
		got_key := key;
		false
	end;
	let rec run () =
		match !got_key with
		| Some k -> k
		| None -> run () in
	run ()

module Colour =
	struct
		type t = Js.js_string Js.t

		let of_string = Js.string
	end

module Style =
	struct
		type t =
			{
				fg : Colour.t option;
				bg : Colour.t option;
			}

		let make ?fg ?bg disp =
			{ fg; bg }

		let default disp =
			{ fg = None; bg = None }
	end

module Base_view =
	struct
		type pos = int * int

		type t =
			{
				win : Window.t;
				elt : Dom_html.canvasElement Js.t;
				con : Dom_html.canvasRenderingContext2D Js.t;
				mutable layouter : Window.t -> unit;
				mutable char_dim : float * float;
				mutable font : Js.js_string Js.t;
				mutable foreground : Colour.t;
				mutable background : Colour.t;
			}

		let dim view =
			let char_dimx, char_dimy = view.char_dim in
			let win_dimx, win_dimy = view.win.Window.dim in
			(
				int_of_float (float_of_int win_dimx /. char_dimx),
				int_of_float (float_of_int win_dimy /. char_dimy)
			)

		let apply_settings view =
			view.con##textAlign <- Js.string "left";
			view.con##textBaseline <- Js.string "top";
			view.con##font <- view.font

		let layout view win =
			let x, y = win.Window.pos in
			let dimx, dimy = win.Window.dim in
			Window.(
				view.elt##style##left <- Js.string (string_of_int x ^ "px");
				view.elt##style##top <- Js.string (string_of_int y ^ "px");
				view.elt##width <- dimx;
				view.elt##height <- dimy;
			);
			apply_settings view

		let config ?(font_pt=12) ?(font_name="monospace") ?(fg=Colour.of_string "#000000") ?(bg=Colour.of_string "#ffffff") view =
			view.font <- Js.string (Printf.sprintf "%ipt %s" font_pt font_name);
			let ch_metrics = view.con##measureText (Js.string "M") in
			(* TODO: can we get the real character proportions? *)
			view.char_dim <- (ch_metrics##width, float_of_int font_pt *. 1.5);
			view.foreground <- fg;
			view.background <- bg

		let make ui win =
			let canvas = Dom_html.createCanvas Dom_html.document in
			Dom.appendChild ui.container_elt canvas;
			canvas##style##position <- Js.string "absolute";
			let con = canvas##getContext (Dom_html._2d_) in
			let view = {
					win = win;
					elt = canvas;
					con = con;
					layouter = (fun _ -> ());
					char_dim = (0., 0.);
					font = Js.string "";
					foreground = Js.string "#0";
					background = Js.string "#0";
				} in
			view.layouter <- layout view;
			config view;
			Window.(
				win.view_layouts <- view.layouter :: win.view_layouts
			);
			view.layouter view.win;
			Js.Opt.iter
				(Dom.CoerceTo.element canvas)
				(fun elt -> ui.view_elts <- elt::ui.view_elts);
			view

		let remove ui view =
			Js.Opt.iter
				(Dom.CoerceTo.element view.elt)
				(fun elt -> ui.view_elts <- List.filter (fun e -> e != elt) ui.view_elts);
			remove_view_elt ui view.elt;
			Window.(
				view.win.view_layouts <- List.filter (fun vl -> vl != view.layouter) view.win.view_layouts
			)

		let refresh view =
			()

		let clear view =
			view.con##fillStyle <- view.background;
			view.con##fillRect (0., 0., float_of_int view.elt##width, float_of_int view.elt##height)
	end

module Chars_view =
	struct
		include Base_view

		let draw view ?style (x, y) char =
			let char_dimx, char_dimy = view.char_dim in
			let sx = float_of_int x *. char_dimx in
			let sy = float_of_int y *. char_dimy in
			begin match style with
			| Some { Style.bg = Some bg; _ } ->
				view.con##fillStyle <- bg;
				view.con##fillRect (sx, sy, char_dimx, char_dimy)
			| _ -> ()
			end;
			view.con##fillStyle <-
				begin match style with
				| Some { Style.fg = Some fg; _ } -> fg
				| _ -> view.foreground
				end;
			view.con##fillText ((Js.string (String.make 1 char)), sx, sy)
	end

module Text_view =
	struct
		include Base_view

		let draw view ?style (x, y) str =
			let char_dimx, char_dimy = view.char_dim in
			let sx = float_of_int x *. char_dimx in
			let sy = float_of_int y *. char_dimy in
			let n = float_of_int (String.length str) in
			begin match style with
			| Some { Style.bg = Some bg; _ } ->
				view.con##fillStyle <- bg;
				view.con##fillRect (sx, sy, char_dimx *. n, char_dimy)
			| _ -> ()
			end;
			view.con##fillStyle <-
				begin match style with
				| Some { Style.fg = Some fg; _ } -> fg
				| _ -> view.foreground
				end;
			view.con##fillText ((Js.string str), sx, sy)
	end
