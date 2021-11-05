let recherche_basique t element = 
    let return = ref false in
    for i = 0 to Array.length t- 1 do 
        if t.(i) = element then return :=true
    done;
    !return
    ;;
    
let recherche_basique_indice t element = 

    let return = ref (-1) in
    for i = 0 to Array.length t -1 do 
        if t.(i) = element then return := i
    done;
    !return;;

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
    
let doublon_tableau t = 
    let return = ref false in
    let t_verif = Array.make (Array.length t) 0 in
    
    for i = 0 to Array.length t -1 do 
        if t_verif.(t.(i)) = 1 then return := true
        else t_verif.(t.(i)) <- 1
    done;
    !return 
    ;;
