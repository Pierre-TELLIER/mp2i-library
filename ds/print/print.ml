(*print [l] display the element [l]*)
let print_l2c l =
    let first = l in 
     
    let rec aux l1 = 
        if l1 == first  then (print_int l1.elm ;print_newline() )
        else (
        print_int l1.elm ;
        print_string ",";
        aux l1.next ;
        )
        
    in aux l.next
        ;;   
        
(*print [l] display the element [l]*)
let print_bool boo = 
    if boo then print_string "true" else print_string "false";
    print_newline()
    ;;

(*print [l] display the element [l]*)
let print_list_int liste = 
    print_string "[" ;
    let rec aux l = match l with 
        | e::e1::q -> (print_int e ); print_string ";" ; aux (e1::q);
        | [e] -> print_int (e) ; print_string "]"
        | _ -> failwith "il n'y a pas d'elements";
    in
    aux liste;
    print_newline ();
    ;;

(*print [l] display the element [l]*)
let print_list_float liste = 
    print_string "[" ;
    let rec aux l = match l with 
        | e::e1::q -> (print_float e ); print_string ";" ; aux (e1::q);
        | [e] -> print_float (e) ; print_string "]"
        | _ -> failwith "il n'y a pas d'elements";
    in
    aux liste;
    print_newline ();
    ;;

(*print [l] display the element [l]*)
let print_zipper zip = 
    print_list_int ((reverse zip.left)@zip.right)
    ;;