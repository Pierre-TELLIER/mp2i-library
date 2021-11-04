(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;


(* [return l] return [l], reversed*)
let reverse l = 
    let rec aux l1 l2 = match l1, l2 with 
    | [],_ -> l2
    | e::q , _-> aux q (e::l2)
    in 
    aux l []
    ;;
    
    
(* [appartient element liste] return [true] if the element is in the list, [false] otherwithe*)
let rec appartient element liste = match liste with
    | [] -> false
    | e::q -> e = element || appartient element q 
    ;;
    
    
(* [appartient element liste] return [true] if the list is sorted, [false] otherwithe*)    
let rec croissant l = match l with 
    | e1::e2::q -> if e1 >= e2 then false else croissant (e2::q) 
    | _ -> true 
    ;;
    
    
(* [dernier l] return the last element of [l]*)
let rec dernier l = match l with 
    | [e] -> e 
    | _::q -> dernier q 
    | _ -> failwith "liste vide"
    ;;
    
(*[deuxieme l] return the second element of l if it exist, or fail *)
let deuxieme l = match l with 
    | e1::e2::q -> e2 
    | _ -> failwith "il n'y a pas de deuxieme element"
    ;;


(*[sumlist l1 l2] return a list of the sum of each element with corresponding one *)
let rec sumlist l1 l2 = match l1,l2 with 
    | [] , [] -> []
    | [] , _ -> l2 
    | _ , [] -> l1 
    | e1::q1, e2::q2 -> (e1 + e2) ::(sumlist q1 q2)
    ;;

(*[concat l1 l2] return a new list, composed by l1 and l2 *)
let concat l1 l2 = 
    let l3 = reverse l1 in 
    
    let rec aux l3  l2= match l3,l2 with 
        | [] , _ -> l2
        | e::q , _ -> aux q (e::l2)
    in
    aux l3 l2
    ;;
    
let maximum l =

    let rec aux l e = match l with 
        | [] -> e 
        | e1::q -> if e1 > e then aux q e1 else aux q e 
    in
    match l with 
        | [] -> failwith "liste vide"
        | e::q -> aux q e  
    ;;

let rec equal l1 l2= match l1,l2 with
    | [],[] -> true
    | [] , _ |_,[] -> false
    | e1::q1, e2::q2 -> if e1 = e2 then equal q1 q2 else false
    ;;
    
    
let lenght l =
    let rec aux l e = match l with 
    | [] -> e
    | _::q -> aux q (e+1)
    in aux l 0
    ;;
    
let rec doublon_suite l = match l with 
    | e1::e2::q -> e1 = e2 || doublon (e2::q)
    | _ -> false
    ;;