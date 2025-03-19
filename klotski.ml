(* klotski.ml: core functionality of the Klotski game. *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with Not_found ->  (* new piece; create a new piece set *)
                let cs = LocSet.singleton (r, c) in
                let p' = CharMap.add ch cs p in
                  iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

(* Part A *)
(* 1. *)
let is_solved board =
  let final_location = LocSet.of_list [(3,1); (3,2); (4,2); (4,1)] in 
  CharMap.exists (fun _ piece -> LocSet.equal piece final_location) board.pieces

(* 2. *)
let compare board1 board2 =
let unocc_locs = LocSet.compare board1.unoccupied board2.unoccupied in 
if unocc_locs != 0 then  unocc_locs 
else 
  let pieces1 = List.map snd (CharMap.bindings board1.pieces) in
  let pieces2 = List.map snd (CharMap.bindings board2.pieces) in 
  LocSetSet.compare (LocSetSet.of_list pieces1) (LocSetSet.of_list pieces2)


(* 3. *)
let remove c board =
match CharMap.find_opt c board.pieces with
| None -> board 
| Some piece ->
    let new_pieces = CharMap.remove c board.pieces in
    let new_unocc = LocSet.union board.unoccupied piece in
    { pieces = new_pieces; unoccupied = new_unocc }


(* 4. *)
let add piece board=
let label = fst (piece) in
let loc = snd (piece) in 
if CharMap.mem label board.pieces then None 
else 
  if LocSet.subset loc board.unoccupied then 
    let new_pieces = CharMap.add label loc board.pieces in 
    let new_occup = LocSet.diff board.unoccupied loc in 
    Some {pieces = new_pieces; unoccupied = new_occup}
else 
  None


(* 5. *)
let make_move (c, d, i) board =
  let step_move c d board =
    match CharMap.find_opt c board.pieces with
    | None -> None
    | Some old_piece ->
        let board_without_c = remove c board in
        let one_step_piece =
          LocSet.map (fun (r, c0) ->
            match d with
            | Up    -> (r - 1, c0)
            | Down  -> (r + 1, c0)
            | Left  -> (r, c0 - 1)
            | Right -> (r, c0 + 1)
          ) old_piece
        in
        add (c, one_step_piece) board_without_c in 
  let rec move_n_times c d n board =
    if n <= 0 then Some board
    else
      match step_move c d board with
      | None -> None
      | Some b' -> move_n_times c d (n - 1) b' in
    
if i < 1 then None
else move_n_times c d i board


(* 6. *)
let next board = 
let directions : dir list = [Up; Down; Left; Right] in 
let piece_moves (c, _)=
  let rec moves_in_dir d i acc = 
    match make_move(c, d, i) board with
    | None -> acc 
    | Some new_board -> moves_in_dir d (i+1) (new_board :: acc)
  in 
  List.concat_map (fun d -> moves_in_dir d 1 []) directions
in 
let pieces = CharMap.bindings board.pieces in 
List.concat (List.map piece_moves pieces)




(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let _test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

