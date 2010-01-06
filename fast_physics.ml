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
open MoreLabels

open Printf
open State
open Constants
open Common

(* Faster physics implementation that is array-based *)
(* Only implements the act_all_on_all function. *)
(* Another useful thing to implement faster would be collision detection *)

(* all-float record, to let the compiler unbox in a loop *) 
type fbody = { mutable x_pos: float;
	       mutable y_pos: float;
	       mutable x_vel: float;
	       mutable y_vel: float;
	       fb_radius: float;
	       fb_mass: float;
	     }

let cube x = x *. x *. x
let square x = x *. x
(* let sqrtcube x = sqrt (x *. x *. x)   (* Faster but less flexible
implementaqtion of sqrtcube *) *)

let body_to_fbody body =
  let (x_pos,y_pos) = body.pos
  and (x_vel,y_vel) = body.velocity
  (*and (x_accel,y_accel) = 
    match body.i with None -> (0.,0.) | Some accel -> accel *)
  in
    { x_pos = x_pos;
      y_pos = y_pos;
      fb_radius = body.radius;
      fb_mass = body.mass;
      x_vel = x_vel;
      y_vel = y_vel;
    }

let build_fbody_array bodies = 
  Array.of_list (List.map ~f:body_to_fbody bodies)

(* convert a body and a b to an updated body *)
let fbody_and_body_to_body body b = 
  { body with 
      velocity = (b.x_vel,b.y_vel);
      pos = (b.x_pos,b.y_pos);
  }


(******** List iterator w/index ********)
let rec list_iteri_rec ~f list i = match list with
    [] -> ()
  | hd::tl -> f ~i hd; list_iteri_rec ~f tl (i+1)

let list_iteri ~f list = 
  list_iteri_rec ~f list 0

(**************************************************)

let array_to_bodies bodies array =
  let b_list = Array.to_list array in
    List.map2 ~f:fbody_and_body_to_body bodies b_list

(**********************************************************************)

let timer = MTimer.create ()

let sqrtcube x = sqrt (x *. x *. x)

let act_all_on_all_rk ~bounce bodies =
  let gexp = grav_exp#v in
  let exp = ((1.0 +. gexp)/.2.0) in
  let sqrtcube = 
    if gexp = 2.0 then sqrtcube else (fun x -> x ** exp) in 
  let const = gconst#v in 

  (* t is the time.  This function has no time dependence.
     s is the position is state space
     dsdt is the array of derivatives 
  *)
  let deriv t s dsdt = 
    (* initialize derivatives to 0 *)
    for i = 0 to Array.length dsdt - 1 do dsdt.(i) <- 0. done;  

    for i = 0 to Array.length bodies - 1 do
      (* x and y pos derivative *)
      dsdt.(i*4) <- s.(i*4 + 2);
      dsdt.(i*4 + 1) <- s.(i*4 + 3);
    done;

    for i = 0 to Array.length bodies - 1 do
      for j = i+1 to Array.length bodies - 1 do
	(* compute i's action on j and vice-versa.  That way you only need to
	 * compute the force once.  It's nearly twice as fast that way. *) 
	let x_pos_i = s.(i*4) and y_pos_i = s.(i*4 + 1) in
	let x_pos_j = s.(j*4) and y_pos_j = s.(j*4 + 1) in

	let xdiff = x_pos_i -. x_pos_j
	and ydiff = y_pos_i -. y_pos_j in

	let dist_sq = xdiff *. xdiff +. ydiff *. ydiff in
	let mult = const /. sqrtcube dist_sq in
	let mult_i = -. mult *. bodies.(j).fb_mass
	and mult_j = mult *. bodies.(i).fb_mass
	in

	(* x vel derivative *)
	dsdt.(i*4 + 2) <- dsdt.(i*4 + 2) +. xdiff *. mult_i;
	dsdt.(j*4 + 2) <- dsdt.(j*4 + 2) +. xdiff *. mult_j;

	(* y vel derivative *)
	dsdt.(i*4 + 3) <- dsdt.(i*4 + 3) +. ydiff *. mult_i;
	dsdt.(j*4 + 3) <- dsdt.(j*4 + 3) +. ydiff *. mult_j;

	
	if bounce &&
	  (let total_radius = bodies.(j).fb_radius +. bodies.(i).fb_radius in
	   dist_sq < total_radius *. total_radius )
	then (
	  let total_radius = bodies.(j).fb_radius +. bodies.(i).fb_radius in

	  let mult = -. mult *. bodies.(i).fb_mass *. bodies.(j).fb_mass
		     *. (total_radius *. total_radius /. dist_sq) in
	  let mult_i = -. mult /. bodies.(i).fb_mass 
	  and mult_j = mult /. bodies.(j).fb_mass 
	  in

	  (* x vel derivative *)
	  dsdt.(i*4 + 2) <- dsdt.(i*4 + 2) +. xdiff *. mult_i;
	  dsdt.(j*4 + 2) <- dsdt.(j*4 + 2) +. xdiff *. mult_j;

	  (* y vel derivative *)
	  dsdt.(i*4 + 3) <- dsdt.(i*4 + 3) +. ydiff *. mult_i;
	  dsdt.(j*4 + 3) <- dsdt.(j*4 + 3) +. ydiff *. mult_j;
	)

      done
    done
  in


  let s = 
    Array.init (4 * Array.length bodies)
      ~f:(fun i -> match i mod 4 with
	    | 0 -> bodies.(i / 4).x_pos
	    | 1 -> bodies.(i / 4).y_pos
	    | 2 -> bodies.(i / 4).x_vel
	    | _ -> bodies.(i / 4).y_vel
	 )
  in

  (* dumb_solver s state.delta#v deriv;  *)
  Rk4.step s 0.0 state.delta#v s deriv;   
  
  for i = 0 to Array.length bodies - 1 do
    bodies.(i).x_pos <- s.(4 * i);
    bodies.(i).y_pos <- s.(4 * i + 1);
    bodies.(i).x_vel <- s.(4 * i + 2);
    bodies.(i).y_vel <- s.(4 * i + 3);
  done


let act_all_on_all ~bounce bodies = 
  let array = build_fbody_array bodies in
    act_all_on_all_rk ~bounce array;
    array_to_bodies bodies array
