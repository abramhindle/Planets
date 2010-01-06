open StdLabels
open MoreLabels

open Tk
open Printf
open Common

exception Unimplemented

(****************************************************************)
(**  Utility Functions   ****************************************)
(****************************************************************)
  
let random_chr () =
  let rand = Random.int ((Char.code 'z') - (Char.code 'a')) in
  Char.chr ((Char.code 'a') + rand)

let random_name length = 
  let str = String.create length in
  for i = 0 to length -1 do
    str.[i] <-  random_chr ()
  done;
  str

module StringMap = AugMap.Make(
  struct 
    type t = string 
    let compare = compare 
  end)

(*****************************************************************)
(**  Live Values  ************************************************)
(*****************************************************************)

(*  Values that have a list of callbacks that are called whenever the value 
 *  is updated.  This is useful both for keeping the optionbox up to date, 
 *  and for doing whatever needs to be done to accomodate changing options. 
 *)


class ['a] live_value (init:'a) = 
object (self)
  val mutable name = None
  val mutable value = init
  val mutable callback_list = []

  method set newval = 
    value <- newval;
    List.iter 
      ~f:(fun cb ->
	    try cb value newval 
	    with exn -> debug_msg 
	      (match name with 
		   None -> (sprintf "live_value#set: Callback failed with exn <%s>" (Printexc.to_string exn))
		 | Some name -> (sprintf "live_value#set %s: Callback failed with exn <%s>" name (Printexc.to_string exn))))
      callback_list

  method set_name newname = name <- Some newname
  method v = value
  method get () = value
  method register_callback cb = 
    callback_list <- cb::callback_list
end

let named_live_value name init =
  let v = new live_value init in
  v#set_name name;
  v

(****************************************************)

class live_toggle init = 
object (self)
  inherit [bool] live_value init

  method flip = self#set (not value)
end

exception Option_exists of string


(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

class type ['a] display_type =
object
  method display : live:bool -> ('a Widget.widget -> unit)
  method tk_to_real : unit
  method name : string
end

(*********************************************************************)
(* 'a is the data type, 'b is the widget type *)

class virtual ['a,'b] option ?name ~text ~set:(set:'a->unit) ~get () =
  let name = (match name with
		  None -> random_name 10
		| Some name -> name ) in
object (self)
  val mutable widget = None

  val tk_var = Textvariable.coerce name
  val name = name
  val text = (text : string)

  method virtual build_widget : live:bool -> 'b 
  method display ~live parent =
    ignore (self#build_widget ~live parent);
    match widget with 
	None -> failwith "option#display: widget unexpectedly missing"
      | Some widget ->
	  Pack.configure ~anchor:`W [widget]

  method virtual get_tk : 'a
  method virtual set_tk : 'a -> unit
  method set_real v = set v
  method tk_to_real = set self#get_tk
  method real_to_tk = self#set_tk (get ())
  method name = name
  method text = text

  method upcast = (self :> 'c display_type)
end


(*********************)

class ['b] toggle_option ?name ~text ~set ~get () =
object (self)
  inherit [bool,'b] option ?name ~text ~set ~get ()
    
  method set_tk bool = 
    Textvariable.set tk_var (if bool then "true" else "false")

  method get_tk = 
    let string = Textvariable.get tk_var in
    if string = "true"  then true
    else if string = "false" then false
    else failwith "toggle_option#get_tk: bad string value" 

  method build_widget ~live parent =
    let new_widget = Checkbutton.create  ~name ~text
		       ~onvalue:"true" ~offvalue:"false" parent in
    (if live then 
       Checkbutton.configure ~command:(fun () -> self#tk_to_real) 
	 new_widget);
    widget <- Some new_widget;
    self#real_to_tk;
    new_widget

end

(*********************)

class ['b] int_scale_option ?name ~min ~max ~text ~set ~get () =
object (self)
  inherit [int,'b] option ?name ~text ~set ~get ()
  val min = min
  val max = max

  method min = min
  method max = max
		 
  method get_tk = match widget with 
      None -> failwith ("int_scale_option#get_tk called when " ^
			"no widget exists")
    | Some widget -> 
	if Winfo.exists widget 
	then int_of_float (Scale.get widget)
	else failwith ("int_scale_option#get_tk called when " ^
		       "widget does not exist")
  method set_tk v = match widget with
      None -> ()
    | Some widget -> 
	if Winfo.exists widget 
	then Scale.set widget (float_of_int v)


  method build_widget ~live parent = 
    let new_widget = Scale.create ~name ~label:text 
		       ~orient:`Horizontal ~min ~max parent in
    widget <- Some new_widget;
    self#real_to_tk;
    (if live then Scale.configure  
       ~command:(fun value -> self#set_real 
		   (int_of_float value)) new_widget);
    new_widget

end


(*******************************************************)

class ['b] float_scale_option ?name ~min ~max ?(resolution=1.0) 
  ~text ~set ~get () = 
object (self)
  inherit [float, 'b] option ?name ~text ~set ~get ()
  val min = min
  val max = max

  method min = min
  method max = max
		 
  method get_tk = match widget with 
      None -> failwith ("float_scale_option#get_tk called when " ^
			"no widget exists")
    | Some widget -> 
	if Winfo.exists widget 
	then Scale.get widget
	else failwith ("float_scale_option#get_tk called when " ^
		       "widget does not exist")

  method set_tk v = match widget with
      None -> ()
    | Some widget -> 
	if Winfo.exists widget 
	then Scale.set widget v


  method build_widget ~live parent = 
    let new_widget = Scale.create ~name ~resolution ~label:text 
		       ~orient:`Horizontal ~min ~max parent in
    widget <- Some new_widget;
    self#real_to_tk;
    (if live then Scale.configure  
       ~command:(fun value -> self#set_real value) new_widget);
    new_widget
end

(*******************************************************)

let string_of_float x = 
  let string = string_of_float x in
  if string.[String.length string - 1] = '.' 
  then string ^ "0"
  else string

class ['b] float_entry_option ?name ?(mult=1.1) 
  ~text ~set ~get () =
object (self)
  inherit [float, 'b] option ?name ~text ~set ~get ()

  val mutable entry = None

  method get_tk = match entry with
      None -> failwith ( "float_entry_option#get_tk called " ^ 
			 "when no widget exists" )
    | Some entry ->
	let float = float_of_string (Entry.get entry) in
	let float = if float <= 0.0 then get () else float in
	let string_rep = string_of_float float in
	Entry.delete_range ~start:(`Num 0) ~stop:`End entry;
	Entry.insert entry ~index:(`Num 0) ~text:string_rep;
	float
	  
  method set_tk v = match widget with
      None -> ()
    | Some widget ->
	match entry with
	    None ->  failwith ("float_entry_option#set_tk" ^ 
			       " called when no widget exists") 
	  | Some entry ->
	      if Winfo.exists entry then (
		Entry.delete_range ~start:(`Num 0) 
					    ~stop:`End entry;
		Entry.insert entry ~index:(`Num 0) 
		  ~text:(string_of_float v) 
	      )

  method build_widget ~live parent =
    let frame = Frame.create parent in
    let nentry = Entry.create ~width:6 frame in
    let label = Label.create ~text frame in
    let action ev = 
      if ev.ev_KeySymString = "Return" then
	self#tk_to_real 
      else if ev.ev_KeySymString = "Up" then
	let newval = (mult *. self#get_tk) in
	self#set_real newval;
	self#set_tk newval
      else if ev.ev_KeySymString = "Down" then
	let newval = (self#get_tk /. mult) in
	self#set_real newval;
	self#set_tk newval
    in
    if live then 
      begin 
	bind ~events:[`KeyPress] ~fields:[`KeySymString] 
	  ~action nentry;
	bind ~events:[`FocusOut] ~fields:[`KeySymString] 
	  ~action:(fun ev -> self#tk_to_real) nentry;
      end;
    Pack.configure ~side:`Left [nentry];
    Pack.configure ~side:`Left [label];
    widget <- Some frame;
    entry <- Some nentry;
    self#real_to_tk;
    frame
end



(*******************************************************)

class ['b] float_entry_display ?name ~text ~set ~get () =
object (self)
  inherit [float, 'b] option ?name ~text ~set ~get ()

  val mutable display = None

  method get_tk = raise Unimplemented
		    
  method set_tk v = match widget with
      None -> ()
    | Some widget ->
	match display with
	    None -> failwith ("float_entry_display#set_tk" ^ 
			      " called when no widget exists") 
	  | Some display ->
	      if Winfo.exists display
	      then Label.configure ~text:(sprintf "%8.4f" v) 
		display

  method build_widget ~live parent =
    let frame = Frame.create parent in
    let label = Label.create ~text frame in
    let ndisplay = Label.create frame  
    in
    Pack.configure ~side:`Left [label];
    Pack.configure ~side:`Left [ndisplay];
    widget <- Some frame;
    display <- Some ndisplay;
    self#real_to_tk;
    frame
end

(*******************************************************)

class ['b] int_entry_display ?name ~text ~set ~get () =
object (self)
  inherit [int, 'b] option ?name ~text ~set ~get ()

  val mutable display = None

  method get_tk = raise Unimplemented
		    
  method set_tk v = match widget with
      None -> ()
    | Some widget ->
	match display with
	    None -> failwith ("int_entry_option#set_tk" ^ 
			      " called when no widget exists") 
	  | Some display ->
	      if Winfo.exists display
	      then Label.configure ~text:(sprintf "%d" v) 
		display

  method build_widget ~live parent =
    let frame = Frame.create parent in
    let label = Label.create ~text frame in
    let ndisplay = Label.create frame  
    in
    Pack.configure ~side:`Left [label];
    Pack.configure ~side:`Left [ndisplay];
    widget <- Some frame;
    display <- Some ndisplay;
    self#real_to_tk;
    frame
end

(*******************************************************)

class ['b] void_entry_display ?name ~text () =
object (self)
  inherit [unit, 'b] option ?name ~text 
    ~set:(fun x -> ()) ~get:(fun () -> ())
    ()

  method get_tk = raise Unimplemented
		    
  method set_tk v = ()

  method build_widget ~live parent =
    let frame = Frame.create parent in
    let label = Label.create ~text frame in
    Pack.configure ~side:`Left [label];
    widget <- Some frame;
    frame
end

(*****************************************************************)
(**  Option Box   ************************************************)
(*****************************************************************)

(***********************************************************)
(* Some helper functions to simplify the optionbox class.
 * The reason they are here instead of being inside of optionbox
 * is that putting them outside allows for a greater degree
 * of polymorphism. *)
(************************************************************)
  

let add_option optionbox ?register_cb option =
  (match register_cb with
       None -> ()
     | Some register ->
	 register 
	 (fun oldval newval -> ignore (oldval = newval); 
	    option#set_tk newval));
  optionbox#add_option option#upcast

let add_option_live optionbox lvalue option =
  add_option optionbox ~register_cb:lvalue#register_callback option

(****************************************************************)
(****************************************************************)
(****************************************************************)

class ['a,'b] optionbox toplevel =
object (self)

  val mutable widget = None

  val mutable display_map = StringMap.empty
  val mutable display_names = []

				
  val mutable mapped = false

  val mutable live = true
  method set_liveness bool = live <- bool
			       
  method add_option (option : 'a display_type) =
    if StringMap.has_key option#name display_map then
      raise (Option_exists option#name)
    else
      ( display_map <- StringMap.add ~key:option#name 
	  ~data:option display_map;
	display_names <- option#name::display_names;
      )

  (*************************************************************)
  (* private toggle methods *)

  (*  Called when OK button is pressed to commit all toggles.  
      Not useful in live option dialog. *)
  method private read_options =
    StringMap.iter 
      ~f:(fun ~key ~data:display -> 
	    try display#tk_to_real with Unimplemented -> () 
	 )
      display_map

  method destroy = 
    match widget with
	None -> failwith "Attempt to destroy non-existant widget"
      | Some widget -> destroy widget

  method private display frame live = 
    List.iter 
      ~f:(fun name -> 
	    try 
	      let display = StringMap.find name display_map in
	      display#display ~live frame;
	    with
		Not_found -> failwith ("Options.display_from_map: BUG." ^
				       " name not found in map."))
      (List.rev display_names)

  method mapped = mapped

  method create_dialog ?(title=Lstrings.get `options) 
    ?geometry ?(clas="Option") ?(transient:'b) () = 
    if not mapped then
      begin
	mapped <- true;
	let topwin = Toplevel.create ~takefocus:true ~clas ~name:"options" 
		       toplevel in 
	let frame = Frame.create ~name:"options" topwin in
	let buttons = 
	  if not live then
	    let cancel_button = Button.create ~text:"Cancel" 
				  ~command:(fun () -> destroy topwin)
				  frame
	    and ok_button = Button.create ~text:"Ok" 
			      ~command:(fun () -> self#read_options; 
					  destroy topwin) frame
	    in
	    [cancel_button; ok_button]
	  else
	    let dismiss_button = Button.create ~text:(Lstrings.get `dismiss)
				   ~command:(fun () -> self#read_options; 
					       destroy topwin) frame
	    in [dismiss_button]
	in
	widget <- Some topwin;
	bind ~events:[`Destroy] ~action:(fun ev -> mapped <- false) frame;
	Pack.configure [frame];
	self#display frame live;
	Pack.configure ~side:`Left buttons;
	(match transient with 
	     None -> ()
	   | Some master -> Wm.transient_set topwin ~master);
	(match geometry with
	     None -> ()
	   | Some geometry -> Wm.geometry_set topwin geometry);
	Wm.title_set topwin title
      end
    else
      debug_msg "Attempt to map already-mapped option dialog"
end

