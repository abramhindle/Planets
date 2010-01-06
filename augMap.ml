open StdLabels
open MoreLabels

module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type S =
sig
  type key
  type 'a t
  val empty: 'a t
  val add: key:key -> (data:'a -> ('a t -> 'a t))
  val find: key -> 'a t -> 'a
  val remove: key -> 'a t -> 'a t
  val mem:  key -> 'a t -> bool
  val has_key: key -> 'a t -> bool
  val iter: f:(key:key -> (data:'a -> unit)) -> ('a t -> unit)
  val map: f:('a -> 'b) -> ('a t -> 'b t)
  val mapi: f:(key -> 'a -> 'b) -> ('a t -> 'b t)
  val fold: f:(key:key -> (data:'a -> ('b -> 'b))) -> ('a t -> (init:'b -> 'b))
  val of_list: (key * 'a) list -> 'a t
  val to_list: 'a t -> (key * 'a) list
  val build_index: key list -> int t
  val filter: f:(key:key -> (data:'a -> bool)) -> ('a t -> 'a t)
  val keys: 'a t -> key list
end

module Make(Ord: OrderedType) : (S with type key = Ord.t) = 
struct

  (* create the underlying map module *)
  module UMap = Map.Make(Ord)
  type key = UMap.key
  type 'a t = 'a UMap.t
  let empty = UMap.empty
  let add = UMap.add
  let find = UMap.find
  let remove = UMap.remove
  let mem = UMap.mem
  let iter = UMap.iter
  let map = UMap.map
  let mapi = UMap.mapi
  let fold = UMap.fold

  let has_key key map =
    try 
      let _ = find key map in
	true
    with
	Not_found -> false

  let of_list pairlist = 
    let rec loop pairlist map = 
      match pairlist with
	  [] -> map
	| (key,data)::tl -> loop tl (add key data map)
    in
      loop pairlist empty

  let to_list map = 
    fold ~f:(fun ~key ~data list -> (key,data)::list) map ~init:[]

  (* takes a list with no duplicates, and produces a 
     map from elements of that list to indices into the list *)
  let build_index list = 
    let rec loop list map i = match list with
	[] -> map
      | hd::tl -> loop tl (add ~key:hd ~data:i map) (i+1)
    in
      loop list empty 0

  let keys map =
    fold ~f:(fun ~key ~data list -> key::list) map ~init:[]


  let filter ~f map =
    fold ~f:(fun ~key ~data map -> 
	       if f ~key ~data
	       then add ~key ~data map
	       else map)
      map
    ~init:empty

end


