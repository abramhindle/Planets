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
module Unix = UnixLabels

open Printf
open State
open Genlex

let major_version = 1
let minor_version = 0


let lexer = make_lexer ["("; ","; ")"; "["; "]";
			"pos"; "velocity"; "radius"; "color"; "mass"; "id";
			"zoom"; "center"; "delta"; "body"; "iterations" ;
		       ]

let rec parse_next list = parser
  | [< 'Kwd "zoom"; 'Float x; e = parse_next ((`Zoom x)::list)  >] -> e
  | [< 'Kwd "center"; pair = parse_pair; e = parse_next ((`Center pair)::list) >] -> e
  | [< 'Kwd "delta"; 'Float x; e = parse_next ((`Delta x)::list) >] -> e
  | [< 'Kwd "iterations"; 'Int x; e = parse_next ((`Iter x)::list) >] -> e
  | [< 'Kwd "body"; e1 = parse_body ; e = parse_next (e1::list) >] -> e
  | [< >] -> list
and parse_body = parser
  | [< 'Kwd "["; body = parse_bnext []; 'Kwd "]" >] -> body
  | [< body = parse_bnext [] >] -> body
and parse_pair = parser
    [< 'Kwd "("; 'Float x; 'Kwd ","; 'Float y; 'Kwd ")" >] -> (x,y)
and parse_bnext list = parser
  | [< 'Kwd "pos";         pair = parse_pair;    e = parse_bnext ((`Pos pair)::list)>] -> e
  | [< 'Kwd "velocity";    pair = parse_pair;    e = parse_bnext ((`Velocity pair)::list)>] -> e
  | [< 'Kwd "radius";      'Float x;             e = parse_bnext ((`Radius x)::list)>] -> e
  | [< 'Kwd "color";       'String x;            e = parse_bnext ((`Color x)::list)>] -> e
  | [< 'Kwd "mass";        'Float x;             e = parse_bnext ((`Mass x)::list)>] -> e
  | [< 'Kwd "id";          'Int x;               e = parse_bnext ((`Id x)::list)>] -> e
  | [< >] -> `Body list
  


(* Converting a state description to a state *)
exception Wrong_type
exception Missing of string

let all_matches ~f list =
  let rec all_matches ~partial list = match list with
      [] -> partial 
    | hd::tl ->
	try 
	  all_matches ~partial:((f hd)::partial) tl
	with
	    Wrong_type -> all_matches ~partial tl
  in
    all_matches ~partial:[] list


(* get first match.  If none available, then raise (Missing name) error *)
let rec first_match ~f ~name list = match list with
    [] -> raise (Missing name)
  | hd::tl -> 
      try 
	f hd
      with
	  Wrong_type -> first_match ~f ~name tl


(* get first match.  If no match available, then return default. *)
let rec first_match_default ~f ~name ~default list = match list with
    [] -> default
  | hd::tl -> 
      try 
	f hd
      with
	  Wrong_type -> first_match_default ~f ~name ~default tl


let build_body bdesc = 
  try
    { pos = first_match      
	      ~f:(function `Pos pos -> pos | _ -> raise Wrong_type)                       
	      ~name:"pos" bdesc; 
      velocity = first_match 
		   ~f:(function `Velocity velocity -> velocity | _ -> raise Wrong_type)        
		   ~name:"velocity" bdesc; 
      radius = first_match   
		 ~f:(function `Radius radius -> radius | _ -> raise Wrong_type)              
		 ~name:"radius" bdesc; 
      color = first_match    
		~f:(function `Color (color:string) -> `Color color | _ -> raise Wrong_type) 
		~name:"color" bdesc; 
      mass = first_match     
	       ~f:(function `Mass mass -> mass | _ -> raise Wrong_type)                    
	       ~name:"mass" bdesc; 
      id = first_match       
	     ~f:(function `Id id -> id | _ -> raise Wrong_type)                          
	     ~name:"id" bdesc; 
      i = None
    }
  with 
      Missing name ->
	raise (Missing (sprintf "body: %s" name))


let build_state sdesc = 
  try
    { 
      d_zoom = first_match ~f:(function `Zoom zoom -> zoom  | _ -> raise Wrong_type)                              ~name:"zoom" sdesc;
      d_center = first_match_default ~f:(function `Center center -> center  | _ -> raise Wrong_type) 	          ~name:"center"             ~default:(0.,0.) sdesc;
      d_delta = first_match ~f:(function `Delta delta -> delta  | _ -> raise Wrong_type) 		          ~name:"delta" sdesc;
      d_bodies = List.map ~f:build_body   (all_matches ~f:(function `Body bodies -> bodies | _ -> raise Wrong_type) sdesc);
    }
  with Missing name ->
    failwith (sprintf "Field missing from state description: %s" name)


(********************************************************************)
(********************************************************************)
(********************************************************************)

let parse_state in_c = 
  let token_stream = lexer (Stream.of_channel in_c) in
    build_state (parse_next [] token_stream)

let string_of_float x = sprintf "%.20e" x
let string_of_int x = sprintf "%d" x

let string_of_pair pair = 
  sprintf "(%s, %s)" (string_of_float (fst pair)) 
    (string_of_float (snd pair))

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
  fprintf out_c "zoom   %s\n" (string_of_float state.zoom#v);
  fprintf out_c "center %s\n" (string_of_pair state.center#v);
  fprintf out_c "delta  %s\n" (string_of_float state.delta#v);
  List.iter ~f:(write_body out_c) state.bodies;
  close_out out_c


(* Some final details: 
   choosing the save directory and the external interface *)

let is_dir fname = 
  let stats = Unix.stat fname in
    stats.Unix.st_kind = Unix.S_DIR

let save_directory = 
  try 
    let home = Sys.getenv "HOME" in
    let pdir = Filename.concat home ".planets" in
    if Sys.file_exists pdir & is_dir pdir
    then pdir
    else 
      (* PROBLEM: is 0x1FF really the right mode? *)
      try Unix.mkdir pdir 0x1FF; pdir 
      with Unix.Unix_error (err,func,arg) -> ""
  with
      Not_found -> ""

(******************************************************************)

let write_state_file filename = write_state (open_out filename)
let read_state_file filename = 
  let dead_state = parse_state (open_in filename) in
  reanimate_dead_state dead_state

let saved_fname key = "uni." ^ key

let write_state key =
  let fname = Filename.concat save_directory (saved_fname key) in
  try
    write_state_file fname
  with
      Sys_error x -> 
	Common.debug_msg (sprintf "%s: failed to load file %s" x fname)

let read_state key = 
  let fname = Filename.concat save_directory (saved_fname key) in
  try
    read_state_file fname
  with 
      Sys_error x -> 
	Common.debug_msg (sprintf "%s: failed to load file %s" x fname)
  

(****************************************************************)

let help_fname = Filename.concat save_directory ".nohelp"
let help_start = not (Sys.file_exists help_fname)

let set_help_start x = match x with
    true -> if Sys.file_exists help_fname then Sys.remove help_fname
  | false -> 
      let file = open_out help_fname in
      close_out file
    
