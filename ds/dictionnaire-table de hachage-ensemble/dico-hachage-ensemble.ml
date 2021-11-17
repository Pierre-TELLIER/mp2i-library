(* type abstrait impératif de dictionnaire *)
type ('cle, 'valeur) dict = { get : 'cle -> 'valeur option ; add : 'cle * 'valeur -> unit ; del : 'cle -> unit } ;;

(*definnition d'un dictionnaire particulier*)
let donnees = ref [] 

(*retire un coulpe d'un dictionnaire à partir d'une clé*)
let delete k = donnees := (List.filter (fun x -> fst(x)<> k) !

(*récupère la valeur associée à la clé donnée*)
let recup k = 
    let data = List.filter (fun x -> fst(x) = k) !donnees in
    
    let values = List.map snd data in 
    
    match values with 
        | e::q ->  Some (e)
        | [] -> None

(*ajoute un couple (clé, valeur) au dictionnaire*)
let ajoute (k,v) = match gete k with 
    | None -> donnees := (k,v)::!donnees 
    | Some (x) -> delete k ; donnees := (k,v)::!donnees 


let dicho = { add = ajoute ; del = delete; get = recup }
(*fin du dictionnaire*)



(* trouve l'élément le plus fréquent dans une liste de taille n *)
let frequent t dico =
    donnees := [] ; (*reinitialise le dictionnaire dans le cas ou il n'est pas vide*)
    let inexistant = ref false in 
    let maxi = ref t.(0) in
    let freq_maxi = ref 0 in
    for i = 0 to Array.length t - 1 do
        let freq = match dico.get t.(i) with
            | None -> dico.add (t.(i), 1); 1
            | Some f -> dico.add (t.(i), f + 1); f + 1 in
        if !freq_maxi = freq then
            (inexistant:= true);
        if !freq_maxi < freq then
            (maxi := t.(i); freq_maxi := freq; inexistant:= false);
        
    done ;
    if !inexistant then failwith "il n'y a pas d'elements majoritaire" ;
    !maxi ;;
    

(*definition du type de hashtable, prenant un tableau*)
type ('cle , 'valeur) hashtable = { tableau : ('cle * 'valeur) option array ; hache : 'cle -> int} ;;

(*definition d'une hashtable particulière *)
let ht_get ht cle = ht.tableau.(ht.hache cle) ;;

let ht_add ht (cle , valeur) = ht.tableau.(ht.hache cle) <- Some valeur ;;

let ht_del ht cle = ht.tableau.(ht.hache cle) <- None ;;
(*fin de la definition d'un dicho*)

(*convertit un hashtable en int dict*)
let dict_of_ht n = (* n est la taille du tableau *)
    let ht = {
        tableau = Array.make n None ;
        hache = fun x -> x mod n
    } in
        {
        add = ht_add ht;
        get = ht_get ht;
        del = ht_del ht
        } ;;
        
