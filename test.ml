open Tk
open Printf

let toplevel = openTk ~clas:"Options" () 
let entry = Entry.create toplevel

let action ev = 
  if ev.ev_KeySymString = "Return" then
    begin
      printf "%s" (Entry.get entry);
      print_newline ()
    end

let _ = 
  bind ~events:[`KeyPress] ~fields:[`KeySymString] ~action entry;
  Pack.configure [entry]; 
  mainLoop ()


