(* A painful and boring extension to the Set module.  *)
(* This is basically a somewhat extended and more efficient 
    implementation of Set. *)
(* Extended in that it has of_list, exists and for_all *)
(* More efficient in that cardinal is now (usually)
   O(1) instead of O(n) *)

open StdLabels
open MoreLabels

module type S =
  sig
    type elt
    and t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t  
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t  
    val inter : t -> t -> t  
    val diff : t -> t -> t   
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : f:(elt -> unit) -> (t -> unit)
    val fold : f:(elt -> 'a -> 'a) -> (t -> (init:'a -> 'a))
    val cardinal : t -> int  (* more efficient than original *)
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    (* these are my additions to Set.S *)
    val of_list : elt list -> t
    val exists : (elt -> bool) -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
  end


module MakeFromSet(SomeSet : Set.S ) : (S with type elt = SomeSet.elt) =
struct 
  type t = { set: SomeSet.t;
	     mutable length: int; } 
      (* a length of (-1) implies the length is currently 
	 unknown *)
  type elt = SomeSet.elt

  let unary unary_func s =  unary_func s.set
  let merge merge_func s t = { set = merge_func s.set t.set; length = -1 }
  let join join_func s t = join_func s.set t.set
  let incr v inc = if v >= 0 then v + inc else v


  let empty = { set = SomeSet.empty; length = 0 }
  and is_empty = unary SomeSet.is_empty
  and mem elt s = SomeSet.mem elt s.set
  and add elt s = 
    if SomeSet.mem elt s.set 
    then s
    else { set = SomeSet.add elt s.set; length = incr s.length 1 }
  and singleton elt = { set = SomeSet.singleton elt; length = 1 }
  and remove elt s = 
    if SomeSet.mem elt s.set 
    then { set = SomeSet.remove elt s.set; length = incr s.length (-1) }
    else s

  let union = merge SomeSet.union
  and inter = merge SomeSet.inter
  and diff = merge SomeSet.diff

  and compare = join SomeSet.compare
  and equal = join SomeSet.equal
  and subset = join SomeSet.subset

  and iter ~f s = SomeSet.iter ~f s.set
  and fold ~f s = SomeSet.fold ~f s.set
  and cardinal s = 
    (if s.length < 0 
     then s.length <- SomeSet.cardinal s.set);
    s.length
  and elements = unary SomeSet.elements
  and min_elt = unary SomeSet.min_elt
  and max_elt = unary SomeSet.max_elt
  and choose = unary SomeSet.choose

  let of_list list =
    let add_elem set elem = SomeSet.add elem set in
    let new_set = List.fold_left ~f:add_elem ~init:SomeSet.empty list
    in { set = new_set; length = -1 }
  let exists test s = 
    SomeSet.fold ~f:(fun elt tval -> tval || (test elt)) s.set ~init:true
  let for_all test s = 
    SomeSet.fold ~f:(fun elt tval -> tval && (test elt)) s.set ~init:true
end


module Make = functor (Elt : Set.OrderedType) -> MakeFromSet(Set.Make(Elt))


let test () =
  let module IntSet = Make(struct type t = int let compare = compare end) in
  let passed = ref true in
  let test_cond tval fail_str = if not tval then begin Printf.printf "%s\n" fail_str; passed := false end in
  test_cond ((IntSet.cardinal IntSet.empty) = 0) "Empty set length test failed";

  let set1 = IntSet.union (IntSet.of_list [1;2;3]) (IntSet.of_list [3;4;5])
  and set2 = IntSet.of_list [1;2;3;4;5] in
  test_cond (IntSet.equal set1 set2)  "union equality test failed";
  test_cond ((IntSet.cardinal set1) = (IntSet.cardinal set2)) "union size test failed";

  let set1 = IntSet.inter (IntSet.of_list [1;2;3;4]) (IntSet.of_list [3;4;5;6])
  and set2 = IntSet.of_list [3;4] in
  test_cond (IntSet.equal set1 set2)  "inter equality test failed";
  test_cond ((IntSet.cardinal set1) = (IntSet.cardinal set2)) "inter size test failed";

  let set1 = IntSet.diff (IntSet.of_list [1;2;3;4]) (IntSet.of_list [3;4;5;6])
  and set2 = IntSet.of_list [1;2] in
  test_cond (IntSet.equal set1 set2)  "diff equality test failed";
  test_cond ((IntSet.cardinal set1) = (IntSet.cardinal set2)) "diff size test failed";
  
  test_cond ((IntSet.elements (IntSet.of_list [0;1;2;3;4;5])) = [0;1;2;3;4;5]) "of_list/elements test failed";
  test_cond ((IntSet.max_elt (IntSet.of_list [1;3;0;5;4;2])) = 5) "max_elt test failed";
  test_cond ((IntSet.min_elt (IntSet.of_list [1;3;0;5;4;2])) = 0) "min_elt test failed";

  test_cond (IntSet.subset (IntSet.of_list [1;4;3;4;4]) (IntSet.of_list [1;2;4;65;3;4;6;4])) "Subset/of_list test failed";
  test_cond (IntSet.mem 3 (IntSet.of_list [1;2;5;3;5;6;7])) "mem test failed";
  test_cond ((IntSet.cardinal (IntSet.of_list [1;2;3;1;2;3;3;1]))  = 3) "cardinal test failed";
  !passed
