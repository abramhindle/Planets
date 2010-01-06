open Printf


type fref = { mutable v: float }

let iterations = try int_of_string(Sys.argv.(1)) with _ -> 10000
let x = { v = 1.0 }		   
let y = { v = 1.0 }		   


(* first sqrt *)
let timer = MTimer.create ()

let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 1.0;
    x.v <- sqrt(y.v)
  done;
  MTimer.stop timer
let _ = 
  printf "Sqrt: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


(* Then exponent *)
let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 1.0;
    x.v <- y.v**(0.51)
  done;
  MTimer.stop timer
let _ = 
  printf "Pow:  %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


(* Then exponent *)
let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 1.0;
    x.v <- y.v*.y.v
  done;
  MTimer.stop timer
let _ = 
  printf "Sqr:  %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 100.0;
    x.v <- min x.v y.v
  done;
  MTimer.stop timer
let _ = 
  printf "Min:  %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 100.0;
    x.v <- if x.v < y.v then x.v else y.v
  done;
  MTimer.stop timer
let _ = 
  printf "FMin: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)



let _ =
  let rec loop i partial = match i with
      0 -> ()
    | _ ->  
	if i < partial 
	then loop (i-1) i else loop (i-1) partial
  in
    MTimer.start timer;
    loop iterations (iterations/2);
    MTimer.stop timer
let _ = 
  printf "FMin: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)



let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v *. 1.000000001;
  done;
  MTimer.stop timer
let _ = 
  printf "Prod: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    y.v <- y.v +. 1.0;
  done;
  MTimer.stop timer
let _ = 
  printf "Plus: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


let x = ref 0
let _ =
  MTimer.start timer;
  for i = 0 to iterations - 1 do
    x := !x + i
  done;
  MTimer.stop timer
let _ = 
  printf "IPls: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)


type interval = { low: float; 
		  high: float;
		}

let intersect a b = { low = if a.low > b.low then a.low else b.low;
		      high = if a.high < b.high then a.high else b.high;
		    }

let isempty i = i.low >= i.high
let empty = { low = 1.; high = 0. }
let int1 = { low = 0.0; high = 4.2;}
let int2 = { low = 0.9; high = 3.4;}


(*
let intersect (alow,ahigh) (blow,bhigh) = (max alow blow),(min ahigh bhigh)
let int1 = (0.0,4.2)
let int2 = (0.9, 3.4)
*)
(* Then exponent *)
let rec loop n partial = match n with
    0 -> ()
  | _ -> loop (n - 1) (intersect int1 partial)
let _ =
  MTimer.start timer;
  loop iterations int2;
  MTimer.stop timer
let _ = 
  printf "Intr: %f us per iteration\n" 
    (MTimer.read_us timer /. float iterations)
