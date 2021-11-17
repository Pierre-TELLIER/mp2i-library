type 'a stack_imperative = {
    empty : unit -> bool;
    push : 'a -> unit;
    pop : unit -> 'a
};;

(** [stack_of_array t] returns a stack implemented with array t *)
let stack_of_array t =
  let n = ref 0 in (* number of elemnts in the stack *)
  {
    empty = (fun () -> !n = 0);
    push = (fun e -> if !n >= Array.length t 
            then failwith "Full stack"
            else (t.(!n) <- e; incr n));
    pop = (fun () -> if !n = 0
          then failwith "Empty stack"
          else (decr n; t.(!n)))
  };;

(** [stack_to_list s] converts a stack to a list *)
let stack_to_list s =
  let rec make_list () =
    if s.empty () then []
    else let top = s.pop () in
      top::make_list () in
  let l = List.rev (make_list ()) in
  List.iter s.push l;
  l;;

(* [stack_print s] shows the elements of the stack *)
(* /!\ fonctionne que pour des éléments de type str ici /!\ *)
let stack_print s =
    while not (Stack.is_empty s) do (
        let elem = Stack.pop s in
        print_string elem ;
        print_newline ()
        )
    done;;

(* [hanoi n] shows the sequence of movements necessary to solve the Hanoi turns with n disks *)
let hanoi n =
    let p = Stack.create () in
    Stack.push (n, 0, 2) p;
    while not (Stack.is_empty p) do
        let k, i, j = Stack.pop p in
        if k = 1 then
        begin print_int i; print_string " "; print_int j; print_newline () end
        else let tige_intermediaire = 3-i-j in
        Stack.push (k-1, tige_intermediaire, j) p;
        Stack.push (1, i, j) p;
        Stack.push (k-1, i , tige_intermediaire) p;
    done;;