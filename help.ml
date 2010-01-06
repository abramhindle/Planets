open Tk
open Printf
open Common

let prologue = Lstrings.get `prologue

class window toplevel text = 
object (self)

  val mutable widget = None
  val on_startup = Textvariable.create ()

  method display ?(title=Lstrings.get `help) ?geometry () = 
    match widget with
	None ->
	  debug_msg "Creating help window from scratch";
	  let topwin = Toplevel.create toplevel in
	  let frame1 = Frame.create topwin in
	  let frame2 = Frame.create topwin in
	  let frame3 = Frame.create topwin in
	  let textw = Text.create frame1 in
	  let vscrollbar = Scrollbar.create ~command:(Text.yview textw) 
			     ~orient:`Vertical frame1 in
	  let hscrollbar = Scrollbar.create ~command:(Text.xview textw) 
			     ~orient:`Horizontal frame2 in
	  let cbutton = 
	    Checkbutton.create ~text:(Lstrings.get `at_startup)
	      ~variable:on_startup frame3 
	      ~command:(fun () -> SaveState.set_help_start 
			  (Textvariable.get on_startup = "1"))
	  in

	  Text.insert ~index:(`Atxy (0,0),[]) ~text:(prologue ^ text) textw; 
	  Text.configure ~state:`Disabled ~wrap:`None ~width:65
	    ~yscrollcommand:(Scrollbar.set vscrollbar) 
	    ~xscrollcommand:(Scrollbar.set hscrollbar) textw;
	  widget <- Some topwin;
	  bind ~events:[`Destroy] ~action:(fun ev -> widget <- None) topwin;
	  Pack.configure [frame1] ~side:`Top ~fill:`Both ~expand:true;
	  Pack.configure [frame2] ~fill:`X ~side:`Top;
	  Pack.configure [frame3] ~fill:`X ~side:`Top;
	  Pack.configure [textw] ~side:`Left ~fill:`Both ~expand:true; 
	  Pack.configure [vscrollbar] ~side:`Right ~fill:`Y;
	  Pack.configure [hscrollbar] ~fill:`X;
	  Pack.configure [cbutton];
	  Wm.title_set topwin title;
	  (match geometry with
	       None -> ()
	     | Some geometry -> Wm.geometry_set topwin geometry)

      | Some widget -> 
	  debug_msg "Raising help window";
	  Tk.raise_window widget

  initializer
    if SaveState.help_start 
    then Textvariable.set on_startup "1"
    else Textvariable.set on_startup "0"

end
    


let help_window = ref None
let create_window toplevel text =
  let window = 
    (match !help_window with 
	 None -> 
	   let win = new window toplevel text in
	   help_window := Some win;
	   win
     | Some win -> win) 
  in
  window#display ~geometry:"+0+0" ()
