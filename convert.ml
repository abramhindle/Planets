(*  Planets:  A simple 2-d celestial simulator
    Copyright (C) 2001  Yaron M. Minsky

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

(* When run in a given directory, converts all the uni.[0-9a-z] files from
 * the old to the new format.  
 *)
 

open Printf
open Tk

type body = { pos: float * float;
	      velocity: float * float;
	      radius: float;
	      color: color;
	      mass: float;
	      id: int;
	    }

type state = { mutable zoom:  float;
	       mutable center:  float * float;
	       mutable delta:  float;
	       mutable bodies:  body list;
	     }

let state = { zoom = 0.0;
	      center = (0.0, 0.0);
	      delta = 0.0;
	      bodies = [];
	    }

let load_universe filename = 
  try
    let in_c = open_in_bin filename in
    let nstate = ((Marshal.from_channel in_c):state) in
      state.zoom <- nstate.zoom;
      state.center <- nstate.center;
      state.delta <- nstate.delta;
      state.bodies <- nstate.bodies;
  with 
      Sys_error str ->
	print_string ("error opening file: " ^  str);
	print_newline ();
	()

(******************************************************************)
(******************************************************************)


let string_of_float x = sprintf "%.20e" x
let string_of_int x = sprintf "%d" x

let string_of_pair pair = 
  sprintf "(%s, %s)" (string_of_float (fst pair)) (string_of_float (snd pair))

let string_of_color color = match color with
    `Color string -> string
  | `Black -> "black"
  | `Blue -> "blue"
  | `Red -> "red"
  | `White -> "white"
  | `Green -> "green"
  | `Yellow -> "yellow"


let write_body out_c body =
  let indent = "   " in
    fprintf out_c "\nbody\n";
    fprintf out_c "%spos      %s\n" indent (string_of_pair body.pos);
    fprintf out_c "%svelocity %s\n" indent (string_of_pair body.velocity);
    fprintf out_c "%sradius   %s\n" indent (string_of_float body.radius);
    fprintf out_c "%smass     %s\n" indent (string_of_float body.mass);
    fprintf out_c "%scolor    \"%s\"\n" indent (string_of_color body.color);
    fprintf out_c "%sid       %s\n" indent (string_of_int body.id)

let write_state out_c = 
  fprintf out_c "zoom   %s\n" (string_of_float state.zoom);
  fprintf out_c "center %s\n" (string_of_pair state.center);
  fprintf out_c "delta  %s\n" (string_of_float state.delta);
  List.iter ~f:(write_body out_c) state.bodies;
  close_out out_c

let write_state_file filename = write_state (open_out filename)


let get_files ~f dir =
  let rec loop files = 
    try
      let file = Unix.readdir dir in
	if f file then loop (file::files)
	else loop files
    with
	End_of_file -> files
  in 
    loop []

let is_uni str = 
  let pat = Str.regexp "^uni\.[0-9a-z]$" in
    Str.string_match ~pat str ~pos:0

let main () =  
  let dir = Unix.opendir(".") in
  let files = get_files ~f:is_uni dir in
    List.iter ~f:(fun filename ->
		    load_universe filename;
		    write_state_file filename)
      files

  

let _ = if not !Sys.interactive then main () 
