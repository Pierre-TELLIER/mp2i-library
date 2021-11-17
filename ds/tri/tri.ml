(*sort the list l  by using the tri fusion*)
let rec tri_fusion l = match l with 
    | [] -> []
    | [e] -> [e]
    | _ -> let l1, l2 = split(l) in fusion (tri_fusion l1) (tri_fusion l2)
    ;;

(*sort the table t with bad complexity*)
let tri_basique t = 
    let return = Array.make (Array.length t) 0 in
    
    for i = 0 to Array.length t - 1 do 
        let mini = ref (0,max_int) in 
        for j = 0 to Array.length t - 1 do 
            if snd(!mini) > t.(j) then 
            mini := (j,t.(j))
        done;
        return.(i) <- snd(!mini);
        t.(fst(!mini)) <- max_int;
    done ;
    return
    ;;

(*fast sort of the list l in O( n log(n) ) *)
let rec tri_rapide l = match l with 
    | [] -> []
    | e::q -> let inf,sup = (List.filter (function x->x<e)  q) , (List.filter (function x->x>=e)  q) in (tri_rapide inf)@e::(tri_rapide sup)
    ;;