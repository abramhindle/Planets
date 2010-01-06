(* push and top are O(1).  
   pop and take are O(1) amortized.
   to_list and length are O(n).
*)

exception Empty
type 'a t
val empty : 'a t
val push : 'a -> 'a t -> 'a t
val top : 'a t -> 'a
val pop : 'a t -> 'a * 'a t
val remove : 'a t -> 'a t
val to_list : 'a t -> 'a list
val length : 'a t -> int 
