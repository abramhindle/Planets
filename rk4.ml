let dydx = ref (Array.make 0 0.)
let dym = ref (Array.make 0 0.)
let dyt = ref (Array.make 0 0.)
let yt = ref (Array.make 0 0.)

let make_static_arrays dims = 
  begin
    dydx := Array.make dims 0. ;
    dym := Array.make dims 0. ;
    dyt := Array.make dims 0. ;
    yt := Array.make dims 0. ;
  end

let dims = ref 0  

(** Given values for the variables y[1..n] and their derivatives dydx[1..n]
  known at x, use the fourth-order Runge-Kutta method to advance the solution
  over an interval h and return the incremented variables as yout[1..n],
  which need not be a distinct array from y. The user supplies the routine
  derivs(x,y,dydx), which returns derivatives dydx at x. 
*)
let step ~y ~x ~h ~yout ~derivs = 

  (* We store the static arrays used by the routine globally, so they don't
     get reallocated each call.  The dimensions of the problem are not likely
     to change between calls...  We save 3 Array.make-s at the expense of a
     dereference and a compare. *)

  let n = Array.length y in
  let hh = h *. 0.5 
  and h6 = h /. 6.0 in
  let xh = x +. hh in
  let _ = (if n <> !dims then (dims:=n;make_static_arrays n)) in
  let dydx = !dydx
  and dym = !dym
  and dyt = !dyt
  and yt = !yt 
  in 
  derivs x y dydx ;

  for i = 0 to n-1 do 
    yt.(i) <- y.(i) +. hh *. dydx.(i) 
  done ;
  
  derivs xh yt dyt ;
  
  for i = 0 to n-1 do 
    yt.(i) <- y.(i) +. hh *. dyt.(i) 
  done ;
  
  derivs xh yt dym ; 

  for i = 0 to n-1 do 
    yt.(i) <- y.(i) +. h *. dym.(i) ; 
    dym.(i) <- dym.(i) +. dyt.(i)
  done ;
  
  derivs (x +. h) yt dyt ; 
  
  for i = 0 to n-1 do 
    yout.(i) <- y.(i) +. h6 *. (dydx.(i) +. dyt.(i) +. 2.0 *. dym.(i))
  done 


