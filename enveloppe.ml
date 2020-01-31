open Types

exception NoLuck_or_CorruptedData;;

let abs x = if x >= 0 then x else -x;;

let list_length l = 
    let rec aux l acc = match l with 
    |[] -> acc 
    |h::t -> (aux t (acc+1))
   in aux l 0;;     

let tri_fusion inf graphe pivot = 
    let rec diviser l = match l with
    |[] -> [],[]
    |[a] -> [a],[]
    | h1::h2::t ->let l1,l2 = diviser t
                  in (h1 :: l1),(h2 :: l2)
   
    and fusionner inf l1 l2 pivot = match l1,l2 with
    |[],_ -> l2
    | _,[] -> l1
    | h1 :: t1 , h2 :: t2 -> if (inf h1 h2 pivot) then h1 :: (fusionner inf t1 l2 pivot) else h2 :: (fusionner inf l1 t2 pivot)
    in 
    let rec tri l inf pivot = match l with  
    |[] -> []
    |[a]-> [a]
    | l -> let l1,l2 = diviser l in (fusionner inf (tri l1 inf pivot) (tri l2 inf pivot) pivot)
    in tri graphe.points inf pivot;;	
  
let inf_x pa pb = pa.x <= pb.x;;
    
let inf_y pa pb = pa.y <= pb.y;;

let compare pi pj pk = (pj.x - pi.x) * (pk.y - pi.y) - (pj.y - pi.y) * (pk.x - pi.x);;

let inf_agl pi pj pivot = 
    if pi.x <> pivot.x && pj.x <> pivot.x then
        let norm_i = sqrt (float_of_int ((pi.x - pivot.x) * (pi.x - pivot.x) + (pi.y - pivot.y) * (pi.y - pivot.y)))
        and norm_j = sqrt (float_of_int ((pj.x - pivot.x) * (pj.x - pivot.x) + (pj.y - pivot.y) * (pj.y - pivot.y))) in
        let sin_i = (float_of_int (pi.y - pivot.y)) /. norm_i and sin_j = (float_of_int (pj.y - pivot.y)) /. norm_j in sin_i <= sin_j
    else
        pi.x <= pj.x;;

let min_liste liste inf =
    let rec aux liste inf acc cpt = match liste,cpt with
    |[],0 -> failwith("liste vide")
    |[],_ -> acc
    |h::t,0 -> aux t inf h 1
    |h::t,_ when (inf h acc) -> aux t inf h cpt
    |h::t,_ -> aux t inf acc cpt
   in aux liste inf {i = (-1); x = max_int;y = max_int} 0;;

let max_liste liste inf =
    let rec aux liste inf acc cpt = match liste,cpt with
    |[],0 -> failwith("liste vide")
    |[],_ -> acc
    |h::t,0 -> aux t inf h 1
    |h::t,_ when not (inf h acc) -> aux t inf h cpt
    |h::t,_ -> aux t inf acc cpt
   in aux liste inf {i = 0; x = -max_int;y = -max_int} 0;;      
    
let enveloppe_convexe_graham graphe =
    let pivot = (min_liste (graphe.points) (inf_x)) 
    in
    let l_sorted = (tri_fusion inf_agl graphe pivot) 
    in
    let rec traitement l_sorted acc result = match l_sorted,acc,result with
    |[],_,_ -> (result,pivot)
    |h::t,0,[] -> traitement t 1 [h]
    |h::t,1,_ -> traitement t 2 (h::result)
    |h::t,_,p1::p2::q when (compare p2 p1 h >= 0) -> traitement t (acc+1) (h::p1::p2::q);
    |h::t,_,p1::p2::q when (compare p2 p1 h < 0) -> traitement (h::t) (acc+1) (p2::q);
    |_ -> raise NoLuck_or_CorruptedData 
    in traitement l_sorted 0 [];;

let enveloppe_convexe graphe = 
    let result,pivot = enveloppe_convexe_graham graphe in 
    let rec construct graphe result pivot acc = match result,acc with
    |[h],acc -> graphe
    |h1::h2::t,acc when acc.i = (-1) -> construct {points = (graphe.points) ; edges = (h2.i,h1.i)::(h1.i,pivot.i)::(graphe.edges)} (h2::t) pivot h1
    |h1::h2::t,acc -> construct {points = (graphe.points) ; edges = (h2.i,h1.i)::(graphe.edges)} (h2::t) pivot acc
    in construct graphe result pivot {i=(-1); x = 666; y = 666};;

let enveloppe_convexe_2 result pivot = 
  let rec construct edges result pivot acc = match result,acc with
  |[h],acc -> edges
  |h1::h2::t,acc when acc = (-1) -> construct ((h2.i,h1.i)::(h1.i,pivot.i)::edges) (h2::t) pivot (h1.i)
  |h1::h2::t,acc -> construct ((h2.i,h1.i)::edges) (h2::t) pivot acc
  in construct [] result pivot (-1);;
