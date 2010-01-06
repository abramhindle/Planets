(*  Planets:  A simple 2-d celestial simulator
    Copyright (C) 2001-2003  Yaron M. Minsky

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open StdLabels
open MoreLabels

open Tk
open Printf

open State
open Common
open Constants

let gap_ms = Options.named_live_value "gap_ms" 30   
	       (* period, in ms, between callbacks.
                  30ms corresponds to roughly 33 frames/sec *)
let iterations = Options.named_live_value "iterations" (gap_ms#v / 15)
		   (* # iterations per callback *)
let init_screen_width = 500
let init_screen_height = 500

let diameter_multiplier = Options.named_live_value "diameter_multiplier" 1.0
let random_vel_multiplier = Options.named_live_value "random_vel_multiplier" 1.0

let penergy = Options.named_live_value "penergy" 0.0
let kenergy = Options.named_live_value "kenergy" 0.0
let energy = Options.named_live_value "energy" 0.0
let num_bodies = Options.named_live_value "number of bodies" 0

let truebounce = new Options.live_toggle false
let kidmode = new Options.live_toggle false
let _ = 
  truebounce#set_name "truebounce";
  kidmode#set_name "kidmode"

let app_class = "Planets"

(********************************************************)
module IntSet = 
  AugSet.Make(struct type t = int 
		     let compare = compare 
	      end)

(********************************************************)
(***  Color Operations  *********************************)
(********************************************************)

let intensity r g b  = sqrt ((float_of_int r)**2.0 +.
                              (float_of_int g)**2.0 +.
                              (float_of_int b)**2.0)

let max_intensity r g b = 
  let maxval = max (max r g) b in
  let mult = 255.0 /. (float_of_int maxval) in
  let max_r = (float_of_int r) *. mult
  and max_g = (float_of_int g) *. mult
  and max_b = (float_of_int b) *. mult in
    sqrt (max_r ** 2.0 +. max_g ** 2.0 +. max_b ** 2.0)

let test_color r g b = 
  (r >= 0 && r < 256) &&
  (g >= 0 && g < 256) &&
  (b >= 0 && b < 256)

let renormalize r g b =
  let m = max_intensity r g b in
  let i = intensity r g b in
  let new_i = (m +. i) /. 2.0 in
  let r = int_of_float ((float_of_int r) *. new_i /. i)
  and g = int_of_float ((float_of_int g) *. new_i /. i)
  and b = int_of_float ((float_of_int b) *. new_i /. i)
  in 
    assert (test_color r g b);
    rgb r g b

let rand_range () = Random.int 256
let rand_color () = 
  renormalize (rand_range ()) (rand_range ()) (rand_range ())

let change_trace_color id color = 
  try
    let trace = IntMap.find id transient.traces in
      transient.traces <- 
      IntMap.add ~key:id ~data:{trace with t_color = color} transient.traces
  with
      Not_found -> ()

let change_body_color_by_id id = 
  let color = rand_color () in
  let changebodies bodies = 
    List.map ~f:(fun body -> 
		   if body.id = id then
		     { body with color = color }
		   else
		     body)
      bodies
  in
    state.bodies <- changebodies state.bodies;
    change_trace_color id color

let hlcolor = `Yellow
let fgcolor = `White
let bgcolor = `Black

let uw opt = match opt with
    Some x -> x
  | None -> failwith "Display.uw: attempt to unwrap null option"


(****************************************************)
(****************************************************)
(****************************************************)

let compute_energy () = 
  let p = Physics.penergy state.bodies
  and k = Physics.kenergy state.bodies 
  in
  penergy#set (log (abs_float p));
  kenergy#set (log k);
  energy#set (log (abs_float (p +. k)))

(****************************************************)
(****************************************************)
(****************************************************)

type ('a,'b,'c,'d) disp_state = 
    { mutable toplevel: Widget.toplevel Widget.widget option;
      mutable frame:    Widget.frame Widget.widget option;
      mutable canvas:   Widget.canvas Widget.widget option;
      mutable optionbox: ('c,'d) Options.optionbox option;
      mutable dbodies:  'a IntMap.t;
      mutable dtraces:  'b IntMap.t;
      mutable tracked_ids: IntSet.t;
      paused:   Options.live_toggle;
      tracing:  Options.live_toggle;
      tracking: Options.live_toggle;
    }


let disp_state = 
  { toplevel = None;
    frame = None;
    canvas = None;
    optionbox = None;
    dbodies = IntMap.empty;
    dtraces = IntMap.empty;
    tracked_ids = IntSet.empty;
    paused = new Options.live_toggle false;
    tracing = new Options.live_toggle false;
    tracking = new Options.live_toggle false;
  }



let get_dbody id = IntMap.find id disp_state.dbodies
let canvas () = uw disp_state.canvas

let init_optionbox () = 
  let obox = new Options.optionbox (uw disp_state.toplevel) in
    obox#set_liveness true;

    Options.add_option_live obox disp_state.paused
      (new Options.toggle_option 
	 ~text:(Lstrings.get `paused)
	 ~set:disp_state.paused#set
	 ~get:disp_state.paused#get ());

    Options.add_option_live obox disp_state.tracing
      (new Options.toggle_option 
	 ~text:(Lstrings.get `tracing)
	 ~set:disp_state.tracing#set
	 ~get:disp_state.tracing#get ());

    Options.add_option_live obox truebounce
      (new Options.toggle_option 
	 ~text:(Lstrings.get `true_bounce)
	 ~set:truebounce#set
	 ~get:truebounce#get ());


    Options.add_option_live obox transient.bound
      (new Options.int_scale_option 
	 ~text:(Lstrings.get `trace_length)
	 ~set:transient.bound#set
	 ~get:transient.bound#get
	 ~min:3.0 ~max:300.0 ());

    Options.add_option_live obox gap_ms
      (new Options.int_scale_option 
	 ~text:(Lstrings.get `disp_period)
	 ~set:gap_ms#set
	 ~get:gap_ms#get
	 ~min:1.0 ~max:100.0 ());

    Options.add_option_live obox iterations
      (new Options.int_scale_option
	 ~text:(Lstrings.get `iter_display)
	 ~set:iterations#set
	 ~get:iterations#get
	 ~min:1.0 ~max:100.0 ());

    Options.add_option_live obox state.delta
      (new Options.float_entry_option
	 ~text:(Lstrings.get `time_step)
	 ~mult:1.05
	 ~set:state.delta#set
	 ~get:state.delta#get ());

    Options.add_option_live obox gconst
      (new Options.float_entry_option
	 ~text:(Lstrings.get `g)
	 ~mult:1.05
	 ~set:gconst#set
	 ~get:gconst#get ());

    Options.add_option_live obox grav_exp
      (new Options.float_entry_option
	 ~text:(Lstrings.get `grav_exp)
	 ~mult:1.01
	 ~set:grav_exp#set
	 ~get:grav_exp#get ());

    Options.add_option obox  
      (new Options.void_entry_display
	 ~text:"New random bodies:" ());

    Options.add_option_live obox diameter_multiplier
      (new Options.float_entry_option
	 ~text:(Lstrings.get `diam_mult)
	 ~mult:1.01
	 ~set:diameter_multiplier#set
	 ~get:diameter_multiplier#get ());

    Options.add_option_live obox random_vel_multiplier
      (new Options.float_entry_option
	 ~text:(Lstrings.get `rand_vel_mult)
	 ~mult:1.01
	 ~set:random_vel_multiplier#set
	 ~get:random_vel_multiplier#get ());	 

(* Energy Display *)

    Options.add_option_live obox kenergy
      (new Options.float_entry_display
	 ~text:(Lstrings.get `log_k_energy)
	 ~set:kenergy#set
	 ~get:kenergy#get ());

    Options.add_option_live obox penergy
      (new Options.float_entry_display
	 ~text:(Lstrings.get `log_p_energy)
	 ~set:penergy#set
	 ~get:penergy#get ());

    Options.add_option_live obox energy
      (new Options.float_entry_display
	 ~text:(Lstrings.get `log_energy)
	 ~set:energy#set
	 ~get:energy#get ());

    Options.add_option_live obox num_bodies
      (new Options.int_entry_display
	 ~text:(Lstrings.get `num_planets)
	 ~set:num_bodies#set
	 ~get:num_bodies#get ());

    disp_state.optionbox <- Some obox

let toggle_opt_dialog () = 
  match disp_state.optionbox with
      None -> failwith "Display.create_dialog: Attempt to display optionbox when none is available"
    | Some obox -> 
	if obox#mapped 
	then obox#destroy
	else obox#create_dialog ~geometry:"+0+0" ~transient:(uw disp_state.toplevel) ~clas:app_class ()


(********************************************************)
(**  Display Classes  ***********************************)
(********************************************************)

(** Note:  just screwing around here.  Classes are not yet used.   **)
class virtual ['a] display_item tag = 
object (self)  

  val tag = tag
  val mutable alive = true

  method destroy = 
    if not alive then 
      failwith "Attempt to destroy same display_item more than once"
    else
      (Canvas.delete (canvas ()) [tag]; 
       alive <- false)

  method draw item = 
    if not alive 
    then failwith "Display.display_item#draw: attempt to draw deleted item." 
    else self#draw_internal item

  (* Here's where you put the logic for drawing an item*)
  method virtual draw_internal : 'a -> unit
end

(************************************************)

class dbody body tag =
  let tag = tag in
object (self)
  inherit [body] display_item tag

  val id = body.id

  method draw_internal body =
    let r = body.radius *. state.zoom#v
    and (x,y) = real_to_screen body.pos in
    let (x1,y1,x2,y2) = (int_of_float (x -. r), int_of_float (y -. r),
			 int_of_float (x +. r), int_of_float (y +. r))
    in
      Canvas.configure_oval ~fill:body.color ~outline:fgcolor (canvas ()) tag;
      Canvas.coords_set (canvas ()) tag ~xys:[ (x1,y1) ; (x2,y2) ]

  initializer
    begin
      Canvas.bind ~events:[`ButtonPress] ~extend:false ~fields:[] 
	~action:(fun e -> delete_body_by_id id)
		   (* change_body_color_by_id id)  *)
	(canvas ()) tag; 
      self#draw_internal body
    end
end

let new_dbody_with_tag body tag = 
  new dbody body tag

let new_dbody_from_body body =
  let tag = Canvas.create_oval ~x1:0 ~y1:0 ~x2:0 ~y2:0 (canvas ()) in
    new dbody body tag

(************************************************)

class dtrace () = 
  let tag = 
    try Canvas.create_line ~xys:[(0,0);(0,0)] (canvas ()) 
    with e -> failwith (sprintf "line drawing failed: %s" (Printexc.to_string e))
  in
  let _ = Canvas.configure_line ~smooth:false (canvas ()) tag in
object (self)
  inherit [trace] display_item tag

  method draw_internal trace = 
    let screen_trace = 
      List.map ~f:pair_to_int (List.map ~f:real_to_screen (trace_to_list trace))
    in
      if List.length screen_trace > 1 then
        ( Canvas.configure_line ~fill:trace.t_color (canvas ()) tag;
	  Canvas.coords_set (canvas ()) tag ~xys:screen_trace )
end

(**********************************************************)
(**  Classes used in the placement of new planets   *******)
(**********************************************************)

class ['a] new_planet () =
  let tag = Canvas.create_oval ~x1:0 ~y1:0 ~x2:0 ~y2:0 (canvas ()) in
object (self)

  inherit ['a] display_item tag

  val mutable radius = 0.0
  val mutable pos = (0,0)
  val mutable color = fgcolor

  method draw_internal (x,y) =
    let radius = int_of_float radius in
    let coords = [(x-radius,y-radius);(x+radius,y+radius)] in
      Canvas.coords_set (canvas ()) tag ~xys:coords;
      pos <- (x,y)

  method set_radius _radius = 
    radius <- _radius;
    self#draw_internal pos

  method set_color _color =
    Canvas.configure_oval ~fill:_color (canvas ()) tag;
    color <- _color

  method tag = tag
  method pos = pos
  method radius = radius
  method color = color

  initializer
    Canvas.configure_oval ~outline:fgcolor (canvas ()) tag

end

(******)

class ['a] new_velocity planet = 
  let pos = planet#pos in
  let tag = Canvas.create_line ~xys:[pos; pos] ~arrow:`Last (canvas ()) in
  let _ = Canvas.configure_line ~fill:fgcolor  (canvas ()) tag in
object (self) 

  inherit ['a] display_item tag

  val mutable vpos = planet#pos
    
  method draw_internal _vpos = 
    Canvas.coords_set (canvas ()) tag ~xys:[pos; _vpos];
    vpos <- _vpos

  method vpos = vpos
  method tag = tag

end


(*********************************************************)
(**  Functions for selecting bodies  *********************)
(*********************************************************)

let selected_bodies pos1 pos2 = 
  let (x1,y1) = pair_to_float pos1
  and (x2,y2) = pair_to_float pos2
  in
  let x1,x2 = (min x1 x2), (max x1 x2)
  and y1,y2 = (min y1 y2), (max y1 y2)
  in
    List.filter ~f:(fun body -> 
		      let (x,y) = real_to_screen body.pos in
			x1 <= x && x <= x2 &&
			y1 <= y && y <= y2)
      state.bodies



let selected_ids pos1 pos2 = 
  IntSet.of_list
    (List.map ~f:(fun body -> body.id) (selected_bodies pos1 pos2))



let recenter_on_selection pos1 pos2 = 
  debug_msg "recentering on selection";
  let bodies = selected_bodies pos1 pos2 in
    if bodies != [] then 
      begin
	debug_msg (sprintf "recentering on %d bodies" (List.length bodies));
	Physics.zero_speed_bodies bodies;
	Physics.center_bodies bodies
      end
    else
      debug_msg "no bodies selected to recenter on"

let create_rectangle color pos1 pos2 = 
  let (x1,y1) = pos1
  and (x2,y2) = pos2 
  in
    Canvas.create_rectangle ~x1 ~y1 ~x2 ~y2 ~outline:color (canvas ())


(***********************************************************)
(**  Primitives for displaying and deleting display items  *)
(***********************************************************)

let delete_dbody_id id = 
  try
    let dbody = get_dbody id in
      disp_state.dbodies <- IntMap.remove id disp_state.dbodies;
      dbody#destroy
  with Not_found ->
    printf "No dbody with id %d" id;
    print_newline ()

let delete_trace_id id = 
  try
    let dtrace = IntMap.find id disp_state.dtraces in
      disp_state.dtraces <- IntMap.remove id disp_state.dtraces;
      dtrace#destroy
  with Not_found ->
    printf "No dtrace with id: %d" id;
    print_newline ()

(***************************************************) 

(* Lookup functions for dbody/dtrace.  
 * If one is not found, one is created. 
 *)

let get_dbody body = 
  try
    IntMap.find body.id disp_state.dbodies
  with
      Not_found ->
	let dbody = new_dbody_from_body body in
	  disp_state.dbodies <- IntMap.add ~key:body.id ~data:dbody
	    disp_state.dbodies;	
	  dbody

let get_dtrace id = 
  try
    IntMap.find id disp_state.dtraces
  with
      Not_found ->
	let dtrace = new dtrace () in
	  disp_state.dtraces <- IntMap.add ~key:id ~data:dtrace 
	    disp_state.dtraces;	
	  dtrace

(****************)

let draw_body body = (get_dbody body)#draw body
let draw_trace ~key:id ~data:trace = (get_dtrace id)#draw trace
let draw_bodies () = List.iter ~f:draw_body state.bodies
let draw_traces () = IntMap.iter ~f:draw_trace transient.traces  

(****************)

let change_all_body_colors () =
  let changebodies bodies = 
    List.map ~f:(fun body ->  
		   let color = rand_color () in
		     change_trace_color body.id color;
		     { body with color = color} )
      bodies
  in
    state.bodies <- changebodies state.bodies
  

(********************************************************)

let remove_dead_bodies () = 
  let disp_ids = IntSet.of_list (IntMap.keys disp_state.dbodies) in
  let body_ids = IntSet.of_list (List.map ~f:(fun body -> body.id) state.bodies) in
  let dead_ids = IntSet.diff disp_ids body_ids in
    IntSet.iter ~f:delete_dbody_id dead_ids

let remove_dead_traces () =
  let disp_ids = IntSet.of_list (IntMap.keys disp_state.dtraces) in
  let trace_ids = IntSet.of_list (IntMap.keys transient.traces) in
  let dead_ids = IntSet.diff disp_ids trace_ids in
    IntSet.iter ~f:delete_trace_id dead_ids


let remove_all_traces () = 
  let disp_ids = IntSet.of_list (IntMap.keys disp_state.dtraces) in
    IntSet.iter ~f:delete_trace_id disp_ids

let _ = 
  disp_state.tracing#register_callback
    (fun oldval newval ->
       if not newval then remove_all_traces ())

let remove_dead () = 
  remove_dead_bodies ();
  remove_dead_traces ()

(********************************************************)

let redraw_all_basic () = 
  draw_bodies ();
  if disp_state.tracing#v then draw_traces ()

let redraw_all () = 
  redraw_all_basic ();
  remove_dead ()


(*********************************************************)
(*********************************************************)
(*********************************************************)

let pause () = 
  disp_state.paused#set true
  
let resume () = 
  disp_state.paused#set false

let init_body ~color ~pos ~velocity ~radius = 
  let body = { pos = pos;
	       velocity = velocity;
	       radius = radius;
	       color = color;
	       mass = radius ** 3.0;
	       id = Random.bits ();
	       i = None;
	     }
  in 
    set_undo_point ();
    state.bodies <- body::state.bodies;
    set_goback_point ();
    body

let add_new_body ~color ~pos ~vpos ~r =
  let r = r /. state.zoom#v in
    init_body ~color ~pos:(screen_to_real pos) 
    ~velocity:((state.zoom#v *. 10.0) <|>
		 (pair_to_float vpos <-> pair_to_float pos))
    ~radius:r

let add_dbody_from_tag body tag = 
  let dbody = new_dbody_with_tag body tag in
    disp_state.dbodies <- IntMap.add ~key:body.id ~data:dbody disp_state.dbodies


(* track center-of-mass *)
let track_com () = 
  if disp_state.tracking#v then
    let tbodies = List.filter  ~f:(fun body -> 
				     IntSet.mem body.id 
				       disp_state.tracked_ids) 
		    state.bodies
    in
      if tbodies != [] then
	(Physics.zero_speed_bodies tbodies;
	 Physics.center_bodies tbodies)

(*****************************************************)
(*****************************************************)


let file_join filelist =
  let rec file_join filelist partial = match filelist with
      [] -> partial
    | hd::tl -> file_join tl (Filename.concat partial hd)
  in
    file_join filelist ""

(* let toggle b = if b then false else true  *)
let planet_radius i = (float_of_int i) *. 5.0 *. diameter_multiplier#v

let is_num c = 
  (int_of_char '0') <= (int_of_char c) &&
  (int_of_char c) <= (int_of_char '9')

let to_num c = (int_of_char c) - (int_of_char '0')

(* Center weighted random float *)
let cw_rand_float level f = 
  let x = Random.float 2.0 in
  let x = x -. 1.0 in
  let mult =
    if x >= 0.0 
    then (x**level +. 1.0) /. 2.0
    else (-.abs_float(x**level) +. 1.0) /. 2.0
  in
    mult *. f
    
let cw_rand_int level i =
  let f_val = cw_rand_float level (float i) in
    round f_val

let random_velocity size = 
  let screen_vel = ( (Random.float (2.0 *. size)) -. size,
		     (Random.float (2.0 *. size)) -. size )
  in
    random_vel_multiplier#v <*> (state.zoom#v <|> screen_vel)

let random_planet () =
  let screen_radius = planet_radius 3
  and screen_pos = (Random.float (float !screen_width),
		    Random.float (float !screen_height))
  and velocity = random_velocity 0.9
  and color = rand_color () 
  in
  let pos = screen_to_real_float screen_pos 
  and radius = screen_radius /. state.zoom#v
  in
    ignore (init_body ~color ~pos ~velocity ~radius)


let mag v = sqrt (dot v v)
let tweak_size = 0.1
let rand_tweak size = 
  ((Random.float 2.0) -. 1.) *. size *. tweak_size

let rdist ?(n=6) low high = 
  let x = ref 0.0 in
  for i = 0 to n -1 do
    x := !x +. Random.float (1.0/. float n)
  done;
  low +. (high -. low) *. !x
  

(** adds a planet at what would be a near-orbital velocity 
  if the force induced on the new planet all came from a 
  single planet. *)
let orbital_planet dir = 
  let screen_radius = 
    planet_radius (Random.int 3 + 1) in
  let radius = screen_radius /. state.zoom#v 
  in
  let color = rand_color ()  in
  let screen_pos = (rdist ~n:3 0. (float !screen_width),
		    rdist ~n:3 0. (float !screen_height)) in

  let pos = screen_to_real_float screen_pos in
  let ovelocity = 
    Physics.induced_orbital_velocity state.bodies ~pos dir  in
  
  let speed = mag ovelocity in
  let velocity = ovelocity +| (rand_tweak speed,rand_tweak speed) 
  in ignore (init_body ~color ~pos ~velocity ~radius)


(*******************************************************)
(**   Callbacks    *************************************)
(*******************************************************)
type key_matcher = | Key of string  
		   | KeySym of string 
		   | KeyList of string list
		   | Other
type key_handler = { key: key_matcher;
		     description: string; 
		     handler: eventInfo -> unit;
		   }


let rec lookup handlers key = 
  match handlers with
  | handler::tl -> 
      if handler.key = key
      then Some handler
      else lookup tl key
  | [] -> None

let handler_to_string h = 
  let matcher = match h.key with
      Key s -> s
    | KeySym s -> s
    | KeyList slist -> String.concat ", " slist 
    | Other -> "Any other key"
  in
  "\t" ^ matcher ^
  (if String.length matcher > 8 then "" else "\t") ^
  "\t " ^ h.description
    
let hlist_to_string hlist = 
  let slist = List.map ~f:handler_to_string hlist in
  String.concat "\n" slist

(***********************************************************)

let rec get_next_key ~f =
  let handle e = 
    f e.ev_Char;
    set_normal_key_handler ()
  in
  bind_class ~events:[`KeyPress] ~extend:false 
    ~fields:[`Char] ~action:handle app_class

(***********************************************************)
(**  UI State Machine   ************************************)
(***********************************************************)
(*
Events:  Motion, ButtonRelease, ButtonPress, KeyPress


  A: default_state:
       def_key_map
       "a" -> B:create_planet
       ButtonPress -> D:draw_rectangle
       Motion -> nothing
       ButtonRelease -> nothing

  B: create_planet:
       Save Pause, pause
       [0-9] -> change_planet_size
       Motion -> move planet
       BottonPress -> place planet, C:set_planet_direction
       ButtonRelease -> nothing (can't be reached anyway)

  C: set_planet_direction:
       [0-9] -> change_planet_size
       Motion -> move arrow
       ButtonPress -> set arrow, restore pause state, A:default_state
       ButtonRelease -> nothing

  D: draw_rectangle:
       save pause, pause
       No keymap
       ButtonPress -> nothing
       ButtonRelease -> restore pause A:default_state
       Motion -> move rectangle

*)

(***********************************************************)
(**  Planet Select  ****************************************)
(***********************************************************)

and select_planet_handler e =
  clear_select_planet_handler ();
  clear_key_handler ();
  let pos = (e.ev_MouseX,e.ev_MouseY) in
  let pos1 = ref pos  and pos2 = ref pos in
  let tag = create_rectangle fgcolor !pos1 !pos2 in

  let move e =
    pos2 := (e.ev_MouseX,e.ev_MouseY);
    Canvas.coords_set (canvas ()) tag ~xys:[!pos1; !pos2]

  and finish pause_state e = 
    Canvas.delete (canvas ()) [tag];
    disp_state.tracked_ids <- selected_ids !pos1 !pos2;
    disp_state.paused#set pause_state;
    disp_state.tracking#set true;
    restore_normal_bindings ();
    redraw_all ()
  in

  let old_pause_state = disp_state.paused#v in
    disp_state.paused#set true;
    bind ~events:[`Motion] ~extend:false ~fields:[`MouseX;`MouseY] 
    ~action:move (uw disp_state.canvas);
    bind ~events:[`ButtonRelease] ~extend:false 
    ~fields:[] ~action:(finish old_pause_state)
      (uw disp_state.canvas)

and set_select_planet_handler () =
  bind ~events:[`ButtonPress] ~extend:false ~fields:[`MouseX; `MouseY] 
  ~action:select_planet_handler 
    (uw disp_state.canvas)

and clear_select_planet_handler () =
  bind ~events:[`ButtonPress] (uw disp_state.canvas)

(***********************************************************)
(**  Add Planet   ******************************************)
(***********************************************************)

and add_planet pos rad_int =

  let rec move_planet planet e = 
    planet#draw (e.ev_MouseX,e.ev_MouseY)

  and set_radius planet e =
    if String.length e.ev_Char > 0 && is_num e.ev_Char.[0] 
    then 
      let rad_int = to_num e.ev_Char.[0] in
	planet#set_radius (planet_radius rad_int)
    else if e.ev_KeySymString="Down" then
      planet#set_radius (planet#radius *. (1.0/.1.1))
    else if e.ev_KeySymString="Up" then
      planet#set_radius (planet#radius *. 1.1)

  and move_velocity velocity e =
    velocity#draw (e.ev_MouseX,e.ev_MouseY)

  and set_place planet pause_state e =
    planet#draw (e.ev_MouseX,e.ev_MouseY);
    planet#set_color (rand_color ());
    let velocity = new new_velocity planet 
    in
      bind ~events:[`Motion] ~extend:false ~fields:[ `MouseX; `MouseY]  ~action:(move_velocity velocity)
	(uw disp_state.canvas); 
      bind ~events:[`ButtonPress] ~extend:false ~fields:[] ~action:(finish pause_state planet velocity)    
	(uw disp_state.canvas)

  and finish pause_state planet velocity e =
    bind ~events:[`Motion] ~extend:false (uw disp_state.canvas);
    bind ~events:[`ButtonPress] ~extend:false (uw disp_state.canvas);
    restore_normal_bindings ();
    disp_state.paused#set pause_state;
    velocity#destroy;
    let body = add_new_body ~color:planet#color ~pos:planet#pos ~vpos:velocity#vpos ~r:planet#radius in
      add_dbody_from_tag body planet#tag

  in
  let old_pause_state = disp_state.paused#v in
  let planet = new new_planet () in
    planet#set_radius (planet_radius rad_int);
    planet#draw pos;
    disp_state.paused#set true;
    bind ~events:[`Motion] ~extend:false ~fields:[ `MouseX; `MouseY] ~action:(move_planet planet)   
      (uw disp_state.canvas);
    bind_class ~events:[`KeyPress] ~extend:false ~fields:[ `Char; `KeySymString ] ~action:(set_radius planet)  
      app_class;
    bind ~events:[`ButtonPress] ~extend:false ~fields:[`MouseX; `MouseY]
      ~action:(set_place planet old_pause_state)   
      (uw disp_state.canvas);
    bind ~events:[`ButtonRelease] ~extend:false ~fields:[] 
      (uw disp_state.canvas)
      

(***********************************************************)
(***********************************************************)

and keyhandlers = 
  [ 
    { key = Key "H";
      description = (Lstrings.get `display_help );
      handler = 
	(fun e ->
	   Help.create_window (uw disp_state.toplevel) 
	   (hlist_to_string keyhandlers));
    };

    { key =  Key "a";
      description = (Lstrings.get `add_planet );
      handler = (fun e -> 
		   add_planet (e.ev_MouseX,e.ev_MouseY) 3);
    };

    { key = KeyList ["plus";"equal";"KP_Add"];
      description = (Lstrings.get `zoom_in );
      handler = (fun e ->
		   state.zoom#set (state.zoom#v *. 1.1);
		   redraw_all_basic ());
    };
    { key = KeyList ["minus";"underscore"; "KP_Subtract"];
      description = (Lstrings.get `zoom_out );
      handler = (fun e -> 
		   state.zoom#set (state.zoom#v /. 1.1);
		   redraw_all_basic ());
    };

    { key = Key "b";
      description = (Lstrings.get `toggle_true_bounce);
      handler = (fun e -> truebounce#flip;)
    };

    { key = KeyList ["c"; "space"];
      description =  (Lstrings.get `center);
      handler = (fun e ->
		   disp_state.tracking#set false;
		   Physics.zero_speed ();
		   Physics.center ();
		   clear_all_traces ();
		   redraw_all_basic ())
    };
    { key = Key "k";
      description = (Lstrings.get `option_dialog);
      handler = (fun e -> toggle_opt_dialog ())
    };
    { key = Key "o";
      description = (Lstrings.get `change_all_colors);
      handler = (fun e ->
		   change_all_body_colors ();
		   redraw_all_basic ());
    };
    { key = KeyList [ "q" ; "Escape"] ;
      description = (Lstrings.get `quit);
      handler = (fun e -> exit 0);
    };
    { key = Key "e" ;
      description = (Lstrings.get `reset);
      handler = (fun e ->
		   clear_all_traces ();
		   state.bodies <- [];
		   redraw_all ());
    };
    { key = Key "s";
      description =  (Lstrings.get `save );
      handler = (fun e ->
		   let old_pause_state = disp_state.paused#v in
		     disp_state.paused#set true;
		     get_next_key ~f:(fun key ->
					clear_all_traces ();
					SaveState.write_state key;
					disp_state.paused#set old_pause_state
				     ));
    };
    { key = Key "l";
      description = (Lstrings.get `load);
      handler = (fun e ->
		   let old_pause_state = disp_state.paused#v in
		     disp_state.paused#set true;
		     get_next_key ~f:(fun key ->
					debug_msg "Starting to load universe";
					clear_all_traces ();
					SaveState.read_state key;
					debug_msg (sprintf "Done loading: %d planets" 
						     (List.length state.bodies));
					redraw_all ();
					debug_msg "Done redrawing";
					disp_state.paused#set old_pause_state
				     ));
    };
    { key = Key "u";
      description = (Lstrings.get `undo);
      handler = (fun e ->
		   undo ();
		   clear_all_traces ();
		   redraw_all ())
    };

    { key = Key "g";
      description = (Lstrings.get `goback);
      handler = (fun e ->
		   goback ();
		   clear_all_traces ();
		   redraw_all ())
    };
    { key = Key "p";
      description = (Lstrings.get `toggle_pause);
      handler = (fun e ->
		   disp_state.paused#flip)
    };
    { key = Key "t";
      description = (Lstrings.get `toggle_trace);
      handler = (fun e ->
		   disp_state.tracing#flip)
    };

    { key = Key "d";
      description = (Lstrings.get `double_trace);
      handler = (fun e ->
		   transient.bound#set (min 300 (transient.bound#v * 2)));
    };

    { key = Key "h";
      description = (Lstrings.get `halve_trace );
      handler = (fun e ->
		   transient.bound#set (max 3 (transient.bound#v / 2))
		)
    };

    { key = Key "j";
      description = (Lstrings.get `place_random_orbital );
      handler = (fun e ->
		   orbital_planet (Random.int 2 = 1) )
    };

    { key = Key "J";
      description = (Lstrings.get `place_random_orbital_uni );
      handler = (fun e -> orbital_planet true)
    };

    { key = Key "r";
      description = (Lstrings.get `place_random );
      handler = (fun e ->
		   random_planet () )
    };


    { key = Key "x";
      description = (Lstrings.get `cancel_com );
      handler = (fun e ->
		   disp_state.tracking#flip;
		)
    };

    (* Panning Around *)
    { key = KeySym "Up";
      description = (Lstrings.get `pan_up);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let y_s = y_s -. (float !screen_height)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ());
    };

    { key = KeySym "Down";
      description = (Lstrings.get `pan_down);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let y_s = y_s +. (float !screen_height)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };
    { key = KeySym "Left";
      description = (Lstrings.get `pan_left);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let x_s = x_s -. (float !screen_width)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };
    { key = KeySym "Right";
      description = (Lstrings.get `pan_right);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let x_s = x_s +. (float !screen_width)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };

  ]

and kid_keyhandlers = 
  [ 

    { key = KeyList ["KP_Home"; "KP_Up"; "KP_Prior"; "KP_Left"; "KP_Begin"; 
		     "KP_Right"; "KP_End"; "KP_Down"; "KP_Next"; "KP_Insert"; 
		     "KP_Delete"; "KP_En`ter"; "KP_Add"; "KP_Multiply"; 
		     "KP_Divide"; "Num_Lock"; ];
      description = (Lstrings.get `place_random );
      handler = (fun e ->
		   random_planet () )
    };

    { key = Key "=";
      description = (Lstrings.get `zoom_in );
      handler = (fun e ->
		   state.zoom#set (state.zoom#v *. 1.1);
		   redraw_all_basic ());
    };
    { key = Key "-";
      description = (Lstrings.get `zoom_out );
      handler = (fun e -> 
		   state.zoom#set (state.zoom#v *. 0.9);
		   redraw_all_basic ());
    };


    { key = KeySym "Up";
      description = (Lstrings.get `pan_up);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let y_s = y_s -. (float !screen_height)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ());
    };
    { key = KeySym "Down";
      description = (Lstrings.get `pan_down);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let y_s = y_s +. (float !screen_height)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };
    { key = KeySym "Left";
      description = (Lstrings.get `pan_left);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let x_s = x_s -. (float !screen_width)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };
    { key = KeySym "Right";
      description =  (Lstrings.get `pan_right);
      handler = (fun e ->
		   let (x_s,y_s) = real_to_screen(state.center#v) in
		   let x_s = x_s +. (float !screen_width)/.30.0 in
		   let (x,y) = screen_to_real( pair_to_int (x_s,y_s) ) in
		     state.center#set (x,y);
		     redraw_all_basic ()
		);
    };
    { key = KeySym "space";
      description =  (Lstrings.get `center);
      handler = (fun e ->
		   disp_state.tracking#set false;
		   Physics.zero_speed ();
		   Physics.center ();
		   clear_all_traces ();
		   redraw_all_basic ())
    };
    { key = KeyList ["q";"w";"e";"a";"s";"d";"z";"x";"c"];
      description = (Lstrings.get `change_all_colors);
      handler = (fun e ->
		   change_all_body_colors ();
		   redraw_all_basic ());
    };

    { key = KeyList ["1";"2";"3";"4";"5";"6";"7";"8";"9"];
      description = (Lstrings.get `toggle_trace);
      handler = (fun e ->
		   disp_state.tracing#flip)
    };


    { key = KeySym "Escape";
      description = (Lstrings.get `reset);
      handler = (fun e ->
		   clear_all_traces ();
		   state.bodies <- [];
		   redraw_all ());
    };

    { key = Other;
      description = (Lstrings.get `place_random );
      handler = (fun e -> orbital_planet (Random.int 2 = 0) )
    };
  ]


and main_key_handler e = 
  debug_msg "Key handler";
  Focus.force (uw disp_state.toplevel); 
  let key = e.ev_Char 
  and keysym = e.ev_KeySymString in
  let rec loop handlers = match handlers with
      handler::tl -> 
	if (match handler.key with
		Key hkey -> hkey = key
	      | KeySym hkeysym -> hkeysym = keysym
	      | KeyList hkeys -> List.mem keysym hkeys
	      | Other -> true )
	then
	  ( debug_msg handler.description;
	    handler.handler e; )
	else
	  loop tl
    | [] -> debug_msg ("Other Key: " ^ keysym)
  in
  if kidmode#v 
  then loop kid_keyhandlers
  else loop keyhandlers

and clear_key_handler () =
  bind_class ~events:[`KeyPress] app_class

and set_normal_key_handler () =
  bind_class ~events:[`KeyPress] ~extend:false ~fields:[`Char;`MouseX;`MouseY;`KeySymString] 
    ~action:main_key_handler  app_class

(* sets up all handlers that might be fiddled with *)
and restore_normal_bindings () =  
  set_select_planet_handler ();
  set_normal_key_handler ();
  bind ~events:[`ButtonRelease] (canvas ());
  bind ~events:[`Motion] (canvas ())


(***************)

let rec timer_cb () = 
  let compute_timer = MTimer.create () in
  let full_timer = MTimer.create () in 
    let _ = if not disp_state.paused#v then
      ( 
	MTimer.start full_timer;
	update ();
	MTimer.start compute_timer;
	Physics.simulate ~bounce:truebounce#v  iterations#v;
	compute_energy ();
	num_bodies#set (List.length state.bodies);
	MTimer.stop compute_timer;
	track_com ();
	update_traces ();
	(let old_debugging = !debugging in
	   debugging := false;
	   redraw_all (); 
	   debugging := old_debugging);
	MTimer.stop full_timer;
	print_endline "Tick";
	List.iter (fun body ->
		let (x,y) = body.pos in
		let (xv,yv) = body.velocity in
		print_endline (String.concat " " 
				[
					string_of_int body.id ;
					string_of_float body.mass ;
					string_of_float body.radius ;
					string_of_float x;
					string_of_float y;
					string_of_float xv;
					string_of_float yv;
				]);
	) state.bodies;
	(*
	let compute_time_ms = MTimer.read_ms compute_timer 
	and full_time_ms = MTimer.read_ms full_timer in
      	  debug_msg (sprintf "compute: %f ms, other: %f ms" 
		       compute_time_ms (full_time_ms -. compute_time_ms))
	  *)
      )
    in
    let time_left_ms = 
      max 0 (int_of_float (float_of_int gap_ms#v -. MTimer.read_ms full_timer )) in
    let full_time_ms = MTimer.read_ms full_timer  in 
      Timer.set ~ms:time_left_ms ~callback:timer_cb 


let set_size e = 
  debug_msg "Resizing";
  let width, height = e.ev_Width, e.ev_Height in
    screen_center := float_of_int (width/2), float_of_int (height/2);
    screen_width := width;
    screen_height := height

(******************************************************************)

let grab () = Grab.set ~global:true (uw disp_state.toplevel)
let ungrab () = Grab.release (uw disp_state.toplevel)

(* Toggle the focus mode.  This is the first step towards 
   building a kid-friendly  version *)
let toggle_kidmode () =
  (* transient.bounce#set true; *)
  match Grab.status (uw disp_state.toplevel) with 
      `None -> grab ()
    | `Global | `Local -> ungrab ()

let _ = kidmode#register_callback (fun oldv newv -> toggle_kidmode ())

let init () = 
  disp_state.toplevel <- Some (openTk ~clas:app_class ());
  disp_state.frame <- Some 
    (Frame.create ~width:!screen_width ~height:!screen_height
       (uw disp_state.toplevel));
  disp_state.canvas <- Some 
    (Canvas.create ~width:init_screen_width ~height:init_screen_height (uw disp_state.frame));
  screen_center := pair_to_float (init_screen_height / 2, init_screen_width / 2); 
  if SaveState.help_start 
  then Help.create_window (uw disp_state.toplevel) (hlist_to_string keyhandlers);
  Canvas.configure ~background:bgcolor (canvas ());
  Pack.configure ~expand:true ~fill:`Both [uw disp_state.frame];
  Pack.configure ~expand:true ~fill:`Both [uw disp_state.canvas];
  
  timer_cb ();
  restore_normal_bindings ();

  bind ~events:[`Configure] ~extend:false ~fields:[`Width; `Height] ~action:set_size 
    (canvas ());

  bind_class ~events:[`Modified ([`Control; `Shift], `KeyPressDetail "K")] ~extend:true
    ~action:(fun _ -> kidmode#flip) app_class;

  appname_set "Planets";
  init_optionbox ();
  mainLoop ()
  
let license_notice = sprintf "
Planets %s, Copyright (C) 2001-2003 Yaron M. Minsky
Planets comes with ABSOLUTELY NO WARRANTY; 
This is free software, and you are welcome to redistribute it
under the terms of the GNU GPL; see the COPYING file for details.
" version

let main () =
  print_string license_notice; print_newline ();
  Random.self_init ();
  state.delta#set 0.45;
  state.bodies <- [];
  init ()

let _ = if not !Sys.interactive then main () 
