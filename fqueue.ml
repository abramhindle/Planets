(* Simple implementation of a polymorphic functional queue *)

(* push and top are O(1).  
   pop and take are O(1) amortized.
   to_list and length are O(n).
*)

(* Invariant:  
   if queue is not empty, outlist is not empty  
   queue.length = List.length(queue.outlist) + List.length(queue.inlist)*)

exception Empty

type 'a t = { inlist: 'a list;
	      outlist: 'a list;
	      length: int;
	    }

(*****************************************)

(*
let test_invariants queue = 
  assert 
    begin 
      queue.length = (List.length queue.outlist) + (List.length queue.inlist)
    end;
  assert 
    begin 
      (queue.length = 0) || List.length queue.outlist > 0
    end
*)

let empty = { inlist = [];
	      outlist = [];
	      length = 0;
	    }

(*****************************************)

let push el queue =
  if queue.outlist = [] then
    let outlist = List.rev (el::queue.inlist) 
    in { inlist = []; 
	 outlist = outlist;
	 length = queue.length + 1;
       }
  else
    { inlist = el::queue.inlist;
      outlist = queue.outlist;
      length = queue.length + 1;
    }

(*****************************************)

let top queue = 
  match queue.outlist with
      [] -> (if queue.inlist != [] 
	     then failwith "FQueue.top: BUG. inlist should be empty but isn't"
	     else raise Empty)
    | hd::tl -> hd

(*****************************************)

let pop queue = match queue.outlist with
    hd::[] -> (hd, { inlist = []; 
		     outlist = (List.rev queue.inlist); 
		     length = queue.length - 1})
  | hd::tl -> (hd, { inlist = queue.inlist;
		     outlist = tl;
		     length = queue.length - 1;})
  | [] -> 
      if queue.inlist = [] 
      then raise Empty
      else (match List.rev queue.inlist with
		[] -> failwith "FQueue.top: BUG.  inlist should not be empty here"
	      | hd::tl -> (hd, { inlist=[]; 
				 outlist=tl; 
				 length = queue.length - 1;
			       }))

(*****************************************)

let remove queue = 
  let (el,new_q) = pop queue in
    new_q
      
(*****************************************)

let to_list queue = 
  queue.inlist @ (List.rev (queue.outlist))

(*****************************************)    
  
let length queue = queue.length

