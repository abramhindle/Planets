(*  Planets:  A celestial simulator
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

open Printf
open Tk


module IntMap = AugMap.Make(struct type t = int let compare = compare end)
module IntSet = AugSet.Make(struct type t = int let compare = compare end)

let trunc = int_of_float


(* Basic types *)
type body = { pos: float * float;
	      velocity: float * float;
	      radius: float;
	      color: color;
	      mass: float;
	      id: int;
	      i: (float * float) option; (* extra integration info.  
					    Will be used for Runge-Kutta info *)
	    }

type state = { zoom: float Options.live_value;
	       center: (float * float)  Options.live_value;
	       delta:  float Options.live_value;
	       mutable bodies:  body list;
	     }

type dead_state = { d_zoom: float;
		    d_center: float * float;
		    d_delta: float;
		    d_bodies: body list;
		  }

let state = { zoom = new Options.live_value 1.0;
	      center = new Options.live_value (0.0, 0.0);
	      delta = new Options.live_value 5.0;
	      bodies = [];
	    }


(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

(* Transient state. *)

type trace_point = { t_pos: float * float;
		     t_round: int; 
		   }

type trace = { t_queue: trace_point Fqueue.t;
	       t_color: color;
	     }


let empty_trace color = { t_queue = Fqueue.empty;
			  t_color = color;
			}



type transient = { mutable traces: trace IntMap.t; 
		   mutable trace_round: int;
		   mutable com_trace: trace;
		   bound: int Options.live_value;
		 }

let transient =  { traces = IntMap.empty; 
		   com_trace = empty_trace `Black;
		   trace_round = 0;
		   bound = new Options.live_value 20;
		 }


(************************************************)
(**  Traces  ************************************)
(************************************************)

let set_trace_bound = transient.bound#set

let trace_inc () = 
  transient.trace_round <- transient.trace_round + 1

let trace_push pos trace = 
  { trace with t_queue = Fqueue.push { t_pos = pos;
				       t_round = transient.trace_round; } 
			   trace.t_queue
  }
  
let trace_to_list trace =
  let trace_queue = Fqueue.to_list trace.t_queue in
  List.map ~f:(fun trace_point -> trace_point.t_pos)
    trace_queue

let rec trace_filt trace = 
  try
    let oldest = Fqueue.top trace.t_queue in
      if transient.trace_round - oldest.t_round > transient.bound#v
      then trace_filt { trace with t_queue = Fqueue.remove trace.t_queue }
      else trace
  with 
      Fqueue.Empty -> trace

(*************************************************)
(*************************************************)

let add_to_trace body =
  let trace = 
    try
      IntMap.find body.id transient.traces
    with
      Not_found -> empty_trace body.color
  in
    transient.traces <- 
    IntMap.add ~key:body.id ~data:(trace_push body.pos trace) transient.traces

let add_to_com_trace com =
  transient.com_trace <- trace_push com transient.com_trace

let remove_empty_traces () =
  IntMap.fold ~f:(fun ~key:id ~data:trace map -> 
		    if Fqueue.length trace.t_queue = 0 then map
		    else IntMap.add ~key:id ~data:trace map) 
  transient.traces ~init:IntMap.empty
    
let update_traces () = 
  List.iter ~f:add_to_trace state.bodies;
  trace_inc ();
  transient.traces <- IntMap.map ~f:trace_filt transient.traces;
  transient.traces <- remove_empty_traces ()

(*************************************************)

let clear_trace body = 
  transient.traces <- IntMap.remove body.id  transient.traces

(*************************************************)

let remove_traces ids = 
  transient.traces <- 
  List.fold_left
  ~f:(fun map id -> IntMap.remove id map)
  ~init:transient.traces ids

let clear_all_traces () =
  transient.traces <- (IntMap.map 
			 ~f:(fun trace -> { trace with t_queue = Fqueue.empty})
			 transient.traces);
  transient.traces <- remove_empty_traces ()

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

let screen_center = ref (0.0, 0.0)
let screen_width = ref 500
let screen_height = ref 500

(***************************************************************************)
(**  Undo and Goback support  **********************************************)
(***************************************************************************)

let reanimate_dead_state dstate =
  state.zoom#set dstate.d_zoom;
  state.center#set dstate.d_center;
  state.delta#set dstate.d_delta;
  state.bodies <- dstate.d_bodies


let copy_state state =
  { d_zoom = state.zoom#v;
    d_center = state.center#v;
    d_delta = state.delta#v;
    d_bodies = state.bodies;
  }

(* Two separate quees are kept, one for goback, one for undo *)

let goback_states = ref []
let undo_states = ref []

(* calls to set_undo_point and set_goback_point should always be paired *)
let set_undo_point () = 
  undo_states := (copy_state state)::!undo_states

let set_goback_point () =
  goback_states := (copy_state state)::!goback_states
  
let undo () = 
  match !undo_states with
      [] -> ()
    | hd::tl -> 
	reanimate_dead_state hd;
	undo_states := tl;
	match !goback_states with
	    [] -> failwith "State.undo: BUG. laststates should not be empty"
	  | hd::tl -> goback_states := tl

let goback () =
  match !goback_states with
      [] -> ()
    | s::tl -> 
	reanimate_dead_state s


(********************************************************)
(********************************************************)
(********************************************************)

let vzero = (0.,0.)
let add_vect (x1,y1) (x2,y2) = (x1 +. x2, y1 +. y2)
let sub_vect (x1,y1) (x2,y2) = (x1 -. x2, y1 -. y2)
let sc_mult scalar (x,y) = (scalar *. x, scalar *. y)
let sc_div scalar (x,y) = (x /. scalar, y /. scalar)

(* Define the following as infix operators, to make it easier to read *)
let ( <*> ) scalar vect = sc_mult scalar vect      (* scalar mult *)
let ( <|> ) scalar vect = sc_div scalar vect       (* scalar division *)
let ( <+> ) v1 v2 = add_vect v1 v2                 (* vector addition *)
let ( <-> ) v1 v2 = sub_vect v1 v2                 (* vector addition *)
let ( <.> ) (x1,y1) (x2,y2) = x1 *. x2 +. y1 *. y2   (* dot product *)

let ( *| ) scalar vect = sc_mult scalar vect      (* scalar mult *)
let ( /| ) scalar vect = sc_div scalar vect       (* scalar division *)
let ( +| ) v1 v2 = add_vect v1 v2                 (* vector addition *)
let ( -| ) v1 v2 = sub_vect v1 v2                 (* vector addition *)
let dot (x1,y1) (x2,y2) = x1 *. x2 +. y1 *. y2   (* dot product *)

let rotright (x1,y1) = (-.y1,x1)
let rotleft (x1,y1) = (y1,-.x1)

let print_vect (x,y) = printf "(%3f, %3f)" x y

let vsum vectors = 
  let rec loop vectors sum =  match vectors with
      [] -> sum
    | v::tl -> loop tl (sum <+> v)
  in
    loop vectors (0.0,0.0)

let sum nums = 
  let rec loop nums sum = match nums with
      [] -> sum
    | n::tl -> loop tl (sum +. n)
  in 
    loop nums 0.0


(***********************************************)
(***********************************************)
(***********************************************)
let pair_to_float (x,y) = (float_of_int x, float_of_int y)
let pair_to_int (x,y) = (int_of_float x, int_of_float y)

(* Simple graphics primitves *)

let screen_to_real_float pos = 
  state.center#v <+>
  (state.zoom#v <|> (pos <-> !screen_center))

let screen_to_real pos = 
  screen_to_real_float (pair_to_float pos)



let real_to_screen pos =
  (state.zoom#v <*> (pos <-> state.center#v)) <+> !screen_center


(****************)

let wavg x1 w1 x2 w2 =
  ((x1 *. w1) +. (x2 *. w2)) /. (w1 +. w2)

let round f = truncate (floor (f +. 0.5))

let wavgi x1 w1 x2 w2 = 
  round (wavg 
	   (float_of_int x1) w1
	   (float_of_int x2) w2)

let rgb r g b =
  `Color (sprintf "#%02X%02X%02X" r g b)

let decompose_cint cint = 
  let r = (0xFF0000 land cint) lsr 16 
  and g = (0x00FF00 land cint) lsr 8
  and b = (0x0000FF land cint) lsr 0
  in 
    (r,g,b)

let decompose_color color = 
  let cint = 
    match color with 
	`Color cstr -> int_of_string ("0x" ^ (String.sub cstr ~pos:1 ~len:6)) 
      | `Black -> 0x00000
      | `White -> 0xFFFFFF
      | `Red -> 0xFF0000
      | `Green -> 0x00FF00
      | `Blue -> 0x0000FF
      | `Yellow -> 0xFFFF00
  in
    decompose_cint cint

let join_colors c1 w1 c2 w2 =
  let (r1,g1,b1) = decompose_color c1
  and (r2,g2,b2) = decompose_color c2 in
  let (r,g,b) = (wavgi r1 w1 r2 w2, 
		 wavgi g1 w1 g2 w2, 
		 wavgi b1 w1 b2 w2)
  in
    rgb r g b

(*********************************************************************)


let delete_body_by_id id = 
  set_undo_point ();
  state.bodies <- List.filter ~f:(fun body -> body.id <> id) state.bodies;
  set_goback_point ()

(*********************************************************************)

let print_body body = 
  print_string "pos: ";  print_vect body.pos; print_string "  ";
  print_string "vel: ";  print_vect body.velocity; 
  print_string "rad: ";  printf "%5f" body.radius;
  print_newline ()

let rmult () = Random.float 2.0 -. 1.0

