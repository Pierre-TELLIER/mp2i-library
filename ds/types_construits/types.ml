(*define the type l2c, dor listes doublement chainées*)
type  'a l2c = {mutable prev : 'a l2c ; elm: 'a; mutable next :'a l2c} ;;

(*create [e] return a l2c with the element e*)
let create e = 
    let rec liste = {prev = liste; next = liste; elm = e} in
    liste
    ;;
    
(*del_l2c l remove the element f of the l2c*)
let del_l2c l = 
    l.prev.next <- l.next ;
    l.next.prev <- l.prev ;
    ;;
    
(*add_l2c [l1] [l2] modify a l2c with l1 and l2 combined *)
let add_l2c l1 l2 = 
    l2.prev <- l1.prev;
    l2.next <- l1;
    l1.prev.next <- l2;
    l1.prev <- l2;
    ;;

(*mem_l2c [e] [l] return a bool with the presence of e in l*)
let mem_l2c e l  = 
    let rec aux l1 = 
    if l1 == l && l1.elm <> e then
        true
    else if l1.elm <> e then
        aux l1.next 
    else 
        false
    in
    aux l.next

(*lenght_l2c [l] return the lenght of l *)
let lenght_l2c l  = 
    let rec aux l1 = 
    if l1 == l then 1 
    else  aux l1.next +1
    in
    aux l.next

(*fusion_l2c [l1] [l2] return a fusion of l1 and l2*)
let fusion_l2c l1 l2 = 
    l2.next.prev <- l1;
    l1.next.prev <- l2;
    let temp = l2.next in 
    l2.next <- l1.next;
    l1.next <- temp ;
    l2
    ;;
    
(*define the zipper type*)
type 'a zipper ={left: 'a list; right: 'a list };;    

(*move_zip ["right" or "left"] [zip] move the zipper cursor to the right or to the left*)
let move_zip side zip  = match side with 
    | "right" -> (
        match zip.left with 
            | e::q -> {left = q ; right = e::zip.right}
            | _ -> failwith "liste de gauche vide"
            )
    | "left" -> (
        match zip.right with 
            | e::q -> {left = e::zip.left ; right = q}
            | _ -> failwith "liste de droite vide"
            )
            
    | _ -> failwith "you have to put right or left as the first parametre"
    ;;
    

(*define the liste doublement chainée type*)
type 'a case = { elem : 'a; mutable next : 'a liste1 }
and 'a liste1 = Vide | C of 'a case;;

(*to_list l return a classic list created with the elements of a liste1*)
let rec to_list l = match l with 
    | C (x) -> x.elem::(to_list x.next)
    | Vide -> []
    ;;

(*return the next element of a liste1*)
let step l = match l with 
    | Vide -> Vide 
    | C(x) -> x.next
    ;;

(*add zip elm add the element elm to the ziplist zip*)
let add zip elm = 
    {right = elm::zip.right; left = zip.left}
    ;;
(*remove zip supprime l'element a droite du curseur*)
let remove zip = match zip.left with 
    | e::q -> {left = zip.left ; right = q}
    | _ -> failwith "impossible, il n'y a pas d'elements a droite de la barre du zipper"
    ;;