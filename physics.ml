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

(*******************************************************)
(***  Physics  *****************************************)
(*******************************************************)

let cube x = x *. x *. x
let square x = x *. x 

let magsq (x,y) = x*.x +. y*.y
let mag (x,y) = sqrt (x*.x +. y*.y)
let distsq v1 v2 = magsq (v1 <-> v2)
let dist v1 v2 = mag (v1 <-> v2)


(* return unit vector pointing from (x1,y1) to (x2,y2) *)
let unit_vect v1 v2 = dist v1 v2 <|> (v2 <-> v1)

let update_pos body =
  { body with pos = body.pos <+> (state.delta#v <*> body.velocity) }

let update_pos_bodies bodies =
  List.map ~f:update_pos bodies

let print_body body = 
  let (x,y) = body.pos 
  and (x_v,y_v) = body.velocity
  in 
    printf "pos: (%3f,%3f), vel: (%3f,%3f) " x y x_v y_v

let print_bodies bodies = List.iter ~f:print_body bodies

(***********************************************)
(**  Energy Calculations  **********************)
(***********************************************)

let rec pairfold ~f list ~init = 
  match list with
      [] -> init
    | hd::tl -> 
	let init = List.fold_left 
		     ~f:(fun partial el -> f partial hd el) ~init tl in 
	pairfold ~f tl ~init


(* How to compute potential:  sum of pair energy for all pairs 
   (don't do pairs twice),
   where pair energy is G m_1 * m_2 / d
   
   Only works with grav_exp = 2.0

 *)

let pair_energy b1 b2 = 
  let dist = mag (b1.pos <-> b2.pos) in
  -. gconst#v *. b1.mass *. b2.mass /. dist

(* returns potential energy *)
let penergy bodies = 
  pairfold ~f:(fun e b1 b2 -> e +. pair_energy b1 b2) 
    ~init:0. bodies
  
(* returns kinetic energy *)
let kenergy bodies = 
  List.fold_left ~f:(fun e b -> e +. 0.5 *. b.mass *. magsq b.velocity)
    ~init:0. bodies

let energy bodies = 
  (* penergy bodies +.  *)
  kenergy bodies


(***********************************************)
(***********************************************)
(***********************************************)

let center_of_mass bodies = match bodies with
    [] -> (0.0,0.0)
  | _ ->
      let mpositions = 
	List.map ~f:(fun body -> body.mass <*> body.pos) bodies 
      and masses = List.map ~f:(fun body -> body.mass) bodies 
      in
	(sum masses <|> vsum mpositions)
    

let central_velocity bodies = match bodies with
    [] -> (0.0,0.0)
  | _ ->
      let momenta = List.map 
		    ~f:(fun body -> body.mass <*> body.velocity)
		      bodies 
      and masses = List.map ~f:(fun body -> body.mass) bodies 
      in
	(sum masses <|> vsum momenta)

(***********************************************)

let orbital_velocity bodies ~pos dir =
  (* first compute some global facts about the system *)
  let com = center_of_mass bodies
  and masses = List.map ~f:(fun body -> body.mass) bodies 
  and cv = central_velocity bodies in
  let mass = sum masses 
  in
  (* now we compute the orbital speed *)
  let radius_vect = com -| pos in
  let r = sqrt (dot radius_vect radius_vect) in
  let speed = sqrt(gconst#v *. mass /. r) in
  let uvect = r /| radius_vect in
  let uvect = if dir then rotleft uvect else rotright uvect
  in
  (speed *| uvect) +| cv


let induced_orbital_velocity bodies ~pos dir = 
  if List.length bodies = 0 then (0.0,0.0) else 
    let cv = central_velocity bodies in
    let induced_accel_list = 
      List.map ~f:(fun body -> 
		     let dvect = body.pos -| pos in
		     let d = mag dvect in
		     let uvect = d /| dvect in
		     (body.mass /. (d *. d)) *| uvect 
		  )
	bodies in
    let induced_accel = List.fold_left ~f:( +| ) induced_accel_list 
			  ~init:vzero in
    let total_mass = sum (List.map ~f:(fun body -> body.mass) bodies) in
    let implied_dist = sqrt (total_mass /. mag induced_accel) in
    let implied_uvect = mag induced_accel /| induced_accel in
    let speed = sqrt (gconst#v *. total_mass /. implied_dist) in
    let uvect = (if dir then rotleft implied_uvect 
		 else rotright implied_uvect) in
    (speed *| uvect) +| cv
  


(***********************************************)

let sub_velocity vel body =
  { body with velocity = body.velocity <-> vel; }

let zero_speed_bodies selected_bodies = 
  let velocity = central_velocity selected_bodies in
    state.bodies <- List.map ~f:(sub_velocity velocity) state.bodies
      
let center_bodies selected_bodies = 
  let center = center_of_mass selected_bodies in
    state.center#set center

(***********************************************)

let zero_speed () = zero_speed_bodies state.bodies
let center () = center_bodies state.bodies

let bodies_from_ids ids = 
  List.filter ~f:(fun body -> List.mem body.id ids) state.bodies

let zero_speeds_ids ids = zero_speed_bodies (bodies_from_ids ids)
let center_ids ids = center_bodies (bodies_from_ids ids)

(************************************************************************)
(**  Collision   Detection   ********************************************)
(************************************************************************)

let touch ~mult b1 b2 = 
  let mdist = max b1.radius b2.radius in
    distsq b1.pos b2.pos < mdist *. mdist *. mult *. mult

let join_bodies b1 b2 = 
  { pos = center_of_mass [b1; b2];
    velocity = 
      (b1.mass +. b2.mass) <|>
      ((b1.mass <*> b1.velocity) <+> (b2.mass <*> b2.velocity));
    radius = ((b1.radius ** 3.0) +. (b2.radius ** 3.0))**(1.0/.3.0);
    color = join_colors b1.color b1.mass b2.color b2.mass;
    mass = b1.mass +. b2.mass; 
    id = Random.bits ();
    i = None;
  }

let find_single_collision ~mult b1 bodies = 
  let rec loop b1 bodies examined = match bodies with
      [] -> b1::examined
    | b2::tl -> 
	if touch ~mult b1 b2 
	then loop (join_bodies b1 b2) tl examined
	else loop b1 tl (b2::examined)
  in
    loop b1 bodies []

(* look for a collision.  If you find it, return a body list
   with those two bodies joined.  Otherwise, return the original
   unchanged *)
let rec find_collisions ~mult bodies = match bodies with
    [] -> []
  | b1::tl -> (find_single_collision ~mult b1 (find_collisions ~mult tl))


(************************************************************************)
(**   Simulation   ******************************************************)
(************************************************************************)

let compose f g x = f (g x)
let compose3 f g h x = f (g (h x))
let ident x = x

let rec apply n f x =  match n with 
    0 -> x
  | _ -> apply (n-1) f (f x)

let simulate ?(bounce=false) i = 
  let action = 
    compose
      (Fast_physics.act_all_on_all ~bounce)
      (find_collisions ~mult:(if bounce then 0.5 else 1.0))
  in
  state.bodies <- apply i action state.bodies
