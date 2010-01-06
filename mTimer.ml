type t = { mutable start_time : float;
	   mutable stop_time : float; 
	   mutable running : bool;
	 }

let create () = { start_time = 0.0;
		  stop_time = 0.0;
		  running = false;
		}

let start timer = 
  if timer.running then failwith "Timer started twice in a row."
  else ( timer.start_time <- Unix.gettimeofday ();
	 timer.running <- true )

let stop timer = 
  if not timer.running then failwith "Timer stopped when not running."
  else ( timer.stop_time <- Unix.gettimeofday ();
	 timer.running <- false )

let read timer = 
  if timer.running 
  then failwith "Timer read at wrong time"
  else timer.stop_time -. timer.start_time

let read_ms timer = 1000.0 *. (read timer)
let read_us timer = (1000.0 *. 1000.0) *. (read timer)
  
