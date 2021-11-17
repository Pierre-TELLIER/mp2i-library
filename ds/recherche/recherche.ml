#use "../list/list.ml"
#use "../tri/tri.ml"
#use "../tableau/tableau.ml"
#use "../types_construits/types.ml"

(*has_cycle [l] [] recherche un cycle dans une liste doublement chainée l en complexité n*)
let rec has_cycle l vus = match l with 
    | C (x) -> List.mem x vus  || has_cycle x.next (x::vus)
    | Vide -> false

(*cycle_lc [l] recherche un cycle dans une liste doublement chainée l*)
let cycle_lc l = 

    let rec aux tortue lapin =
        if lapin = Vide
            then false
        else if lapin == tortue 
            then true 
        else 
            aux (step tortue) (step (step lapin))
    in aux  l (step l)
    
    
(*vérifie si un élement est dans un tableau en O(n)*)
let recherche_basique t element = 
    let return = ref false in
    for i = 0 to Array.length t- 1 do 
        if t.(i) = element then return :=true
    done;
    !return
    ;;

(*renvoie l'indice d'un élement dans un tableau (renvoie -1 sinon)*)
let recherche_basique_indice t element = 
    let return = ref (-1) in
    for i = 0 to Array.length t -1 do 
        if t.(i) = element then return := i
    done;
    !return;;

(*recherche d'un élément dans un tableau par dichotomie en O(log)*)
let dichotomie t element =     
    let rec dicho mini maxi = 
        if mini > maxi then -1 else
        let millieu = (mini+maxi) /2 in 
        
        if t.(millieu) = element then millieu
        else if t.(millieu) < element then dicho (millieu+1) maxi  
        else dicho (mini+1) millieu
        
    in
    dicho 0 (Array.length t -1)
    ;;

(*recherche d'un élément dans un tableau par trichotomie en O(log)*)
let trichotomie t element = 
    
    let rec tricho mini maxi = 

        if mini > maxi then -1 else
        
        let millieu1 = mini + ((maxi-mini) /3) in 
        let millieu2 = mini + (2 * ((maxi-mini) /3)) in
        if t.(millieu1) = element then millieu1 else if t.(millieu2) = element then millieu2
        
        else if t.(millieu1) > element then tricho mini (millieu1-1)
        else if t.(millieu2) > element then tricho (millieu1+1) (millieu2-1)
        
        else tricho (millieu2+1) maxi
        
    in
    tricho 0 (Array.length t -1)
    ;;

(*vérifie si il y a au moins deux fois le même élément dans un tableau*)
let doublon_tableau t = 
    let return = ref false in
    let t_verif = Array.make (Array.length t) 0 in
    
    for i = 0 to Array.length t -1 do 
        if t_verif.(t.(i)) = 1 then return := true
        else t_verif.(t.(i)) <- 1
    done;
    !return 
    ;;


(*recherche si il y a une tranche dans le tableau dont la somme vaux le total voulue. dans le cas echéant, renvoit l'indice de debut et l'indice de fin, sinon renvoit -1,-1*)
let tranche_x t total =
    let mini = ref 0 in
    let maxi = ref (Array.length t - 1) in
    let somme = ref (somme_indice !mini !maxi t) in 
    
    while not (!somme = total) && mini<=maxi do
        if !somme < total then incr mini
        else decr maxi ;
        somme := somme_indice !mini !maxi t
    done; 
    if !mini > !maxi then (-1,-1) else 
    !mini,!maxi
    ;;
    
(*renvoie la plus grande valeur de la somme de termes consécutifs du tableau*)    
let tranche_max t = 
    let current = ref t.(0) in
    let best = ref t.(0) in    
    for i = 0 to Array.length t -1 do 
        current := !current + t.(i);
        if !current > !best then best := !current 
    done;
    !best
    ;;