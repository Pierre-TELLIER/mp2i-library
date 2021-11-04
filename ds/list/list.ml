(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;


let reverse l = 
    let rec aux l1 l2 = match l1, l2 with 
    | [],_ -> l2
    | e::q , _-> aux q (e::l2)
    in 
    aux l []
    ;;

let rec appartient element liste = match liste with
    | [] -> false
    | e::q -> e = element || appartient element q 
    ;;
    
    
let rec croissant l = match l with 
    | e1::e2::q -> if e1 >= e2 then false else croissant (e2::q) 
    | _ -> true 
    ;;

let rec dernier l = match l with 
    | [e] -> e 
    | e::q -> dernier q 
    | _ -> failwith "liste vide"
    ;;
    
let rec deuxieme l = match l with 
    | e1::e2::q -> e2 
    | _ -> failwith "il n'y a pas de deuxieme element"
    ;;
    
