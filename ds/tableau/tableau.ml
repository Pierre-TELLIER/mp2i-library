(*return the sum of each elements in the table*)
let somme t =
    let somme = ref 0 in 
    for i = 0 to Array.length t -1 do 
    somme := !somme + t.(i)
    done;
    !somme
    ;;

(*return the sum of elements of indice between "debut" and "fins"*)
let somme_indice debut fin t = 
    let somme = ref 0 in 
    for i = debut to fin do 
    somme := !somme + t.(i) 
    done;
    !somme
    ;;