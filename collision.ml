open State

(***************************************************************)
(**  Collision detection   *************************************)
(***************************************************************)

(* Computes smallest positive solution to the quadratic equation 
   |p + t*v|^2 - dist = 0.  The solution is:

   - v.p +/- sqrt((v.p)^2 - v.v*(p.p - dist))
   ------------------------------------------ 
                     v.v

   if the sqrt term is imaginary, then bail out early (before computing the
   sqrt).  This is a big performance win.

 *)
let next_collision_values p v dist dotpp = 
  let dotvp = dot v p in
  let dotvpsq = dotvp *. dotvp 
  and dotpp = dot p p
  and dotvv = dot v v in
  let pre_sqrt_term = dotvpsq -. dotvv *. (dotpp -. (dist *. dist)) in
    if pre_sqrt_term <= 0. then infinity
    else
      let sqrt_term = sqrt pre_sqrt_term  in
      let soln1 = (-. dotvp -. sqrt_term) /. dotvv
      and soln2 = (-. dotvp +. sqrt_term) /. dotvv
      in
	(* It's important to ignore planets that are currently touching *)
	(* note that soln2 is always greater than soln1 *)
	if soln2 <= 0. then infinity
	else if soln1 > 0. then soln1
	else soln2 


(* computes time of next collision given two bodies *)
let next_collision_bodies b1 b2  =
  let dist = b1.radius +. b2.radius in
  let p = b1.pos <-> b2.pos in
  let v = b1.velocity <-> b2.velocity in
  let dotpp = dot p p in
  if dotpp < dist *. dist then infinity
  else next_collision_values p v dist dotpp

(* computes nearest collision *)

let next_collision bodies =
  let len = Array.length bodies in
  let best = ref (infinity, None) in
  for i = 0 to len - 2 do
    for j = i + 1 to len - 1 do
      let time = next_collision_bodies bodies.(i) bodies.(j) in
      let ( best_time, _ ) = !best in
      if time < best_time then best := (time, Some (i,j));
    done
  done;
  match !best with
      (time,None) -> raise Not_found
    | (time,Some (i,j)) -> (time, (i,j))


(***************************************************************)
(***  Bouncing   ***********************************************)
(***************************************************************)
(*  Computes the bounce of two touching bodies.  It is assumed that the two
    bodies are touching. *)

(* compute new velocities from 1-dimensional bounce problem *)
let bounce_1d ~s1 ~s2 ~m1 ~m2 = 
  let new_s1 = (( m1 -. m2) *. s1 +. 2. *. m2 *. s2) /. (m1 +. m2)
  and new_s2 = (( m2 -. m1) *. s2 +. 2. *. m1 *. s1) /. (m1 +. m2)
  in
  (new_s1,new_s2)


let magsq v = dot v v
let mag v = sqrt (magsq v)
let uvec v = mag v <|> v

let bounce_pair b1 b2 =
  let v1 = b1.velocity and v2 = b2.velocity
  and m1 = b1.mass and m2 = b2.mass
  and p1 = b1.pos and p2 = b2.pos 
  in
  let u =  uvec (p2 <-> p1) in
  (* speed in collision direction *)
  let s1 = dot v1 u and s2 = dot v2 u in 
  (* perpendicular components of velocity *)
  let v1p = v1 <-> (s1 <*> u) and v2p = v2 <-> (s2 <*> u) in
  let new_s1,new_s2 = bounce_1d ~s1 ~s2 ~m1 ~m2 in

  let new_v1 = (new_s1 <*> u) <+> v1p
  and new_v2 = (new_s2 <*> u) <+> v2p
  in
  
  ( { b1 with velocity = new_v1 }, 
    { b2 with velocity = new_v2 } )
		 

(***************************************************************)
(****   Moving Forward  ****************************************)
(***************************************************************)

let move_forward_simple bodies time =
  for i = 0 to Array.length bodies - 1 do
    let body = bodies.(i) in
    bodies.(i) <- 
    { body with pos = body.pos <+> (time <*> body.velocity) }
  done

let rec move_forward_array time bodies =
  try
    let (ctime,(i,j)) = next_collision bodies in
    if ctime > time then 
      move_forward_simple bodies time
    else (
      move_forward_simple bodies ctime; 
      (* move to collision time *)
      let b_i,b_j = bodies.(i),bodies.(j) in
      let b_i,b_j = bounce_pair b_i b_j in
      bodies.(i) <- b_i;
      bodies.(j) <- b_j;
      move_forward_array (time -. ctime) bodies
    )
  with 
      Not_found -> 
	move_forward_simple bodies time


let move_forward time bodies = 
  let bodies = Array.of_list bodies in
  move_forward_array time bodies;
  Array.to_list bodies
