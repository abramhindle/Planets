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

let version = "__VERSION__"

let debugging = ref false
let debug_msg msg = 
  if !debugging then
    (print_string msg;
     print_newline ())

type 'a reference = { mutable v: 'a }
type fref = { mutable fv: float }

let lpush el l = l := el::!l

(** argument parsing code *)

let anonymous = ref []

let spec = [
  ("-debug", Arg.Set debugging, "Turn debugging mode on")
]


let () = Arg.parse spec (fun s -> lpush s anonymous) "planets [-debug]"
