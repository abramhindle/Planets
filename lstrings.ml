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

(* Some ad-hoc localization *)

let english word = match word with
  | `paused -> "Paused"
  | `tracing -> "Tracing"
  | `true_bounce -> "Bouncing (experimental)"
  | `trace_length -> "Trace Length"
  | `disp_period -> "Disp. Period (ms)"
  | `g -> "G"
  | `grav_exp -> "G exp."
  | `diam_mult -> "Average diameter"
  | `rand_vel_mult -> "Average velocity"
  | `log_k_energy -> "Log k energy"
  | `log_p_energy -> "Log p energy"
  | `log_energy -> "Log energy"
  | `num_planets -> "Number of planets"
  | `add_planet -> "Add Planet"
  | `zoom_in -> "Zoom In"
  | `zoom_out -> "Zoom Out"
  | `iter_display -> "iter/display"
  | `time_step -> "Time step"
  | `toggle_true_bounce -> "Toggle true bounce"
  | `toggle_bounce -> "Toggle bounce"
  | `center -> "Center"
  | `option_dialog -> "Display option dialog"
  | `change_all_colors -> "Change all colors"
  | `quit -> "Quit"
  | `reset -> "Reset to empty universe"
  | `save -> "Save Universe"
  | `load -> "Load Universe"
  | `undo -> "Undo (undoes last planet insertion)"
  | `goback -> "Go Back (goes back to last planet insertion)"
  | `toggle_pause -> "Toggle Pause"
  | `toggle_trace -> "Toggle Trace"
  | `double_trace -> "Double Trace Length"
  | `halve_trace -> "Halve Trace Length"
  | `place_random_orbital -> "Place random orbital planet"
  | `place_random_orbital_uni -> "Place random orbital planet (unidirectional)"
  | `place_random -> "Place random planet"
  | `cancel_com -> "Cancel C-O-M tracking"
  | `pan_up -> "Pan Up"
  | `pan_down -> "Pan Down"
  | `pan_left -> "Pan Left"
  | `pan_right -> "Pan Right"
  | `display_help -> "Display this help dialog"
  | `help -> "Help"
  | `dismiss -> "Dismiss"
  | `options -> "Options"
  | `at_startup -> "Display this screen at startup?"
  | `prologue -> "Welcome to Planets!
Planets is a simple orbital planetary simulator.  A short 
introduction to planets can be found at:
   http://planets.homedns.org/getting_started.html

The following is a list of keybindings.  Note that a summary
of keybindings can be found in the KEYBINDINGS.txt file
distributed with this program.

"

let danish word = match word with
  | `paused -> "Pause"
  | `tracing -> "Vis hale"
  | `true_bounce -> "Ægte stød"
  | `trace_length -> "Halelængde"
  | `disp_period -> "Opdateringstid (ms)"
  | `g -> "G (Tyngdeacceleration)"
  | `grav_exp -> "G exp."
  | `diam_mult -> "Diam. faktor"
  | `rand_vel_mult -> "Hastigheds-faktor"
  | `log_k_energy -> "Log k energi"
  | `log_p_energy -> "Log p energi"
  | `log_energy -> "Log energi"
  | `add_planet -> "Tilføj planet"
  | `zoom_in -> "Zoom ind"
  | `zoom_out -> "Zoom ud"
  | `iter_display -> "Trin/opdatering"
  | `time_step -> "Tidsintervaller"
  | `toggle_true_bounce -> "Ægte stød til/fra"
  | `toggle_bounce -> "Stød til/fra"
  | `center -> "Centrér"
  | `option_dialog -> "Vis dialogboksen indstillinger"
  | `change_all_colors -> "Ændr alle farver"
  | `quit -> "Afslut"
  | `reset -> "Genstart med tomt univers"
  | `save -> "Gem univers"
  | `load -> "Indlæs univers"
  | `undo -> "Fortryd (oprettelsen af sidste planet)"
  | `goback -> "Gå tilbage (til oprettelsen af sidste planet)"
  | `toggle_pause -> "Pause/kør"
  | `toggle_trace -> "Vis/skjul hale"
  | `double_trace -> "Fordobl halens længde"
  | `halve_trace -> "Halvér halens længde"
  | `place_random_orbital -> "Opret planet i tilfældig bane"
  | `place_random -> "Opret tilfældig planet"
  | `cancel_com -> "Afbryd sporing af massemidtpunkt"
  | `pan_up -> "Op"
  | `pan_down -> "Ned"
  | `pan_left -> "Venstre"
  | `pan_right -> "Højre"
  | `display_help -> "Vis denne hjælpeskærm"
  | `help -> "Hjælp" (* "Help" *)
  | `dismiss -> "Anvend" (* "Dismiss" *)
  | `options -> "Indstillinger" (* "Options" *)
  | `at_startup -> "Vis denne hjælpeskærm hver gang Planets starter?"
      (* "Display this screen at startup?" *)  
  | `prologue -> "Velkommen til Planets!
  
Planets er en simpel planetbanesimulator.
Du kan finde en kort (engelsksproget) introduktion til Planets på:
http://planets.homedns.org/getting_started.html

Det følgende er en liste over tastaturgenveje (de er vigtige i dette 
program!).
Bemærk at du kan finde en oversigt over genvejstasterne (en.)
i filen KEYBINDINGS.txt som fulgte med dette program.

"
  | _ -> raise Not_found

let maxsub string ~pos ~len =
  let len = min len (String.length string - pos) in
  String.sub string ~pos ~len

let rec list_find ~f list = match list with
  | [] -> raise Not_found
  | hd::tl -> 
      match (try Some (f hd) with Not_found -> None)
      with
	  Some x -> x
	| None -> list_find ~f tl

let get_locale () = 
  list_find ~f:Sys.getenv ["LC_ALL";"LC_MESSAGES";"LANG"]

let get word = 
  try (
    match maxsub ~pos:0 ~len:2 (Sys.getenv "LANG") with 
      | "en" -> english word
      | "da" -> danish word
      | _ -> english word
  ) with
      Not_found -> english word
