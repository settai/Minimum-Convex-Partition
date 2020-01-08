open Types
open Triangle


exception Error;; 

let cdt = fun arrete list_polygones points -> 
    let list_poly = ref list_polygones in
    let list_edges = List.concat (List.map poly_to_edges !list_poly) in
    match arrete with 
        (a,b) -> let p = List.filter (exists_in_poly a b) !list_poly in
    match p with
        p1::p2::_ -> let poly = poly_fusion p1 p2 in
                    if (is_convex points poly) then true else false
        |_ -> false;;

let fusion_base_arrete = fun arrete list_polygones points ->
    let list_poly = ref list_polygones in
    let list_edges = List.concat (List.map poly_to_edges !list_poly) in
    match arrete with 
        |(a,b) -> let p = List.filter (exists_in_poly a b) !list_poly in
                        begin 
                        match p with
                        p1::p2::_ ->
                            let poly = poly_fusion p1 p2 in
                            if (is_convex points poly) then list_poly := (poly::(remove_poly p2 (remove_poly p1 !list_poly)));
                            !list_poly
                        | _ -> !list_poly
                        end;;

let compte_arrete_enlevable = fun liste_arrete  list_polygones points ->
    let rec aux = fun seq list_poly acc pts->
        match seq with 
        [] -> acc
        |p::q -> if (cdt p list_poly pts) then aux q list_poly (acc+1) pts else aux q list_poly acc pts
     in aux liste_arrete list_polygones 0 points ;;


let arrete_localement_fav = fun liste_de_polygones listes_d_arrete points ->
    let rec aux = fun lp la la_bis pts accNb accEd ->
        match la with 
        [] -> accEd
        |arrete::q -> if ((cdt arrete lp pts) && ((compte_arrete_enlevable (virer_doublon la_bis [arrete]) (fusion_base_arrete arrete lp pts) pts) > accNb )) then aux lp q la_bis pts (compte_arrete_enlevable (virer_doublon la_bis [arrete]) (fusion_base_arrete arrete lp pts) pts) arrete else aux lp q la_bis pts accNb accEd
     in aux liste_de_polygones listes_d_arrete listes_d_arrete points (-1) (-1,-1);;

let rec optim_locale = fun liste_de_polygones listes_d_arrete points ->
    let arrete_enleve = arrete_localement_fav liste_de_polygones listes_d_arrete points in
    if arrete_enleve=(-1,-1) then liste_de_polygones else optim_locale (fusion_base_arrete arrete_enleve liste_de_polygones points) (virer_doublon listes_d_arrete [arrete_enleve]) points;;
        
    

