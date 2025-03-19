(* gsearch.ml: generic searching over a domain *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end
  

(* Part B *)
module Search (S : Storage) (D : Domain) =
struct
  module DS = Set.Make(D)
  exception FoundSolution of D.t list

  let search init =
    let storage = S.create () in
    S.push [init] storage;               
    let visited = ref DS.empty in         
    try
      while not (S.is_empty storage) do
        let history = S.pop storage in
        let current_board = List.hd history in
        if DS.mem current_board !visited then
          ()
        else if D.is_solved current_board then
          raise (FoundSolution history)
        else begin
          visited := DS.add current_board !visited;
          let successors = D.next current_board in
          List.iter (fun b -> S.push (b :: history) storage) successors;
        end
      done;
      raise Not_found 
    with
    | FoundSolution hist -> hist

  let show_history hist =
    (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
end
