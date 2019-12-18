open Types;;

(*type figure = {points: point list; edges: point*point list};;*)

exception NoLuck_or_CorruptedData;;

(*let abs x = if x >= 0 then x else -x;;*)

(*let list_length l = 
    let rec aux l acc = match l with 
    |[] -> acc 
    |h::t -> (aux t (acc+1))
   in aux l 0;;     *)

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
(*
let modelise_graph graphe =
    let n = (list_length graphe.edges) in let graphe_model = (Array.make (n + 1) []) in 
    let rec parcours l_edges l_points graphe_model = match l_edges with
    |[] -> graphe_model
    |(p1,p2)::t -> p2::(graphe_model.(p1.i));p1::(graphe_model.(p2.i))
    in parcours graphe.edges graphe.points graphe_model;;
*)   
let inf_x pa pb = pa.x <= pb.x;;
    
(*let inf_y pa pb = pa.y <= pb.y;;*)

let compare pi pj pk = (pj.x - pi.x) * (pk.y - pi.y) - (pj.y - pi.y) * (pk.x - pi.x);;

let inf_agl pi pj pivot = 
    if (pi.x <> pivot.x) && (pj.x <> pivot.x) then
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

(*let max_liste liste inf =
    let rec aux liste inf acc cpt = match liste,cpt with
    |[],0 -> failwith("liste vide")
    |[],_ -> acc
    |h::t,0 -> aux t inf h 1
    |h::t,_ when not (inf h acc) -> aux t inf h cpt
    |h::t,_ -> aux t inf acc cpt
   in aux liste inf {i = 0; x = -max_int;y = -max_int} 0;;      *)
    
let enveloppe_convexe_graham graphe =
    let pivot = (min_liste (graphe.points) (inf_x)) in
    let l_sorted = (tri_fusion inf_agl graphe pivot) in
    let rec traitement l_sorted acc result = 
        match l_sorted,acc,result with
         [],_,_ -> (result,pivot)
        |h::t,0,[] -> traitement t 1 [h]
        |h::t,1,_ -> traitement t 2 (h::result)
        |h::t,_,p1::p2::q -> if ((compare p2 p1 h) >= 0) then  traitement t (acc+1) (h::p1::p2::q) else traitement (h::t) (acc+1) (p2::q);
        |_ -> raise NoLuck_or_CorruptedData
    in traitement l_sorted 0 [];;

let enveloppe_convexe graphe = 
    let result,pivot = enveloppe_convexe_graham graphe in 
    let rec construct graphe result pivot = match result with
    |h::[] -> {points = (graphe.points) ; edges = (h.i, pivot.i)::(graphe.edges)}
    |h1::h2::t -> construct {points = (graphe.points) ; edges = (h1.i,h2.i)::(graphe.edges)} (h2::t) pivot
    in construct graphe result pivot;;
    
(*     
let cherche_convexe_iter fig m =
    let min_x,max_x,min_y,max_y = (min_liste fig.points inf_x),(max_liste fig.points inf_x),(min_liste fig.points inf_y),(max_liste fig.points inf_y) and flag = ref true and i = ref 0 in
    while flag && !i < m do
        let (x: float) = Random.float(max_x - min_x) + min_x and (y: float) = Random.float(max_y - min_y) + min_y in
        let rec traitement_h l y acc_h = match l,acc_h with
        |[],2 -> flag := true
        |[],i when i <= 1 -> raise  NoLuck_or_CorruptedData
        |[],_ -> flag:= false
        |(p1,p2)::t -> if p1.y <= y <= p2.y then traitement_h t y (acc_h+1) else traitement_h l x y acc_h
        in aux traitement_h                 
      
test*)

let a = { i=1 ; x=0 ; y=0 };;
let b = {i=2;x=1;y=2};;
let c = {i=3;x=3;y=4};;
let d = {i=4;x=1;y=1};;
let e = {i=5;x=2;y=2};;
let f = {i=6;x=2;y=1};;
let g = {i=7;x=3;y=1};;
let h = {i=8;x=5;y=(-1)};; 

let l_points = [a;b;c;d;e;f;g;h];;  

let graphe_test = {points = l_points; edges= [] };; 

enveloppe_convexe_graham graphe_test ;;

    
(*     
let recup_extremum_trie liste = 
    let rec aux liste acc mini maxi r_list = match liste,acc with
    |[],1 -> failwith("liste vide")
    |[a],1 -> (aux [] (acc+1) a a [])
    |h::t,1 -> (aux t 2 a maxi [])
    |h::t,_ -> (aux t (acc+1) mini maxi h::r_list)
    |[a],acc when acc >= 2 -> (aux [] (acc+1) mini a r_list)
    |[],acc when acc >= 2 ->  (aux [] -1 mini maxi (rev r_list))
    |[],-1 -> r_list;mini;maxi
    in aux liste 1 (-max_int) max_int [];; 


*)   
