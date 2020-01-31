open Stack
open Types
(*open Enveloppe*)


exception Pite

let list_length l = 
    let rec aux l acc = match l with 
    |[] -> acc 
    |h::t -> (aux t (acc+1))
   in aux l 0;; 

(*let make_vect pts_graph_l = Array.of_list pts_graph_l;;*)

let modelise_graph l_edges (pts_tab:point_graph array) =
    let n = (Array.length pts_tab) in let graphe_model = (Array.make (n) [])
    in 
    let rec parcours l_edges (pts_tab:point_graph array) (graphe_model:point_graph list array) = match l_edges with
    |[] -> graphe_model
    |(p1,p2)::t -> let l1 = graphe_model.(p1) and pts = pts_tab.(p2) in graphe_model.(p1) <- pts::l1;
                   let l2 = graphe_model.(p2) and pts = pts_tab.(p1) in graphe_model.(p2) <- pts::l2; 
                   parcours t pts_tab graphe_model 
    in parcours l_edges pts_tab graphe_model;;

let dfs (graphe_model:point_graph list array) =
    let n = Array.length graphe_model in 
    let dejavu = (Array.make n false) and atraiter = Stack.create()
    in
    let s = Random.int (n-1)
    in  
    Stack.push s atraiter; dejavu.(s) <- true; 
    let rec parcours l cpt (graphe_model:point_graph list array) dejavu atraiter = match l,cpt with
    |[],0 -> false
    |[h],0 -> false 
    |[],_ -> begin try 
                let ind = (Stack.pop atraiter) in (parcours graphe_model.(ind) 0 graphe_model dejavu atraiter) 
             with 
             Stack.Empty -> let i=(ref 0) and flag=(ref true) in
                            while (!i <= (n - 1)) && !flag do
                                flag := (!flag) && (dejavu.(!i));
                                i := !i + 1
                            done; 
                            !flag
             end;  
    |h::t,_ when dejavu.(h.i) -> (parcours t (cpt+1) graphe_model dejavu atraiter) 
    |h::t,_ -> (Stack.push (h.i) atraiter); dejavu.(h.i) <- true ;(parcours t (cpt+1) graphe_model dejavu atraiter)
    in parcours graphe_model.(s) 0 graphe_model dejavu atraiter;;

let dfs_test graphe_model =
    let n = Array.length graphe_model in 
    let dejavu = (Array.make n false) and atraiter = Stack.create()
    in
    let s = Random.int (n-1)
    in  
    Stack.push s atraiter; dejavu.(s) <- true;
    Printf.printf "IND : %d\n" s;
    Printf.printf "Booleen : %B\n" dejavu.(s);  
    let rec parcours l cpt graphe_model dejavu atraiter = match l,cpt with
    |[],0 -> Printf.printf "%s \n" "herecas1"; false
    |[h],0 -> Printf.printf "%s \n" "herecas2"; false 
    |[],_ -> Printf.printf "%s\n" "here2";
             begin try 
                let ind = (Stack.pop atraiter) in (parcours graphe_model.(ind) 0 graphe_model dejavu atraiter) 
             with 
             Stack.Empty -> Printf.printf "%s\n" "la est le pb";let i=(ref 0) and flag=(ref true) in
                            while (!i < (n - 1)) && !flag do
                                flag := (!flag) && (dejavu.(!i));
                                Printf.printf "num : %d" !i;
                                Printf.printf "iter : %B" !flag; 
                                i := !i + 1
                            done; !flag
             |_ -> Printf.printf "%s\n" "stp";false
             end   
    |h::t,_ ->  Printf.printf "Inv = %d" (h.i); 
                if not dejavu.(h.i) then begin 
                    (Stack.push (h.i) atraiter); 
                    dejavu.(h.i) <- true; 
                    Printf.printf "Push : %d \n" (h.i); 
                    (parcours t (cpt+1) graphe_model dejavu atraiter) 
                end
                else 
                (parcours t (cpt+1) graphe_model dejavu atraiter) 
    in parcours graphe_model.(s) 0 graphe_model dejavu atraiter;;

(*let conflit a1 a2 (pts_tab:point_graph array) =
  let ida,idb = a1 and idc,idd = a2 in
  let pta = pts_tab.(ida) and ptb = (pts_tab.(idb)) and ptc = (pts_tab.(idc)) and ptd = (pts_tab.(idd)) in
  let m1 = (float_of_int(pta.y - ptb.y))/.(float_of_int(pta.x - ptb.x)) and m2 = (float_of_int(ptc.y - ptd.y))/.(float_of_int(ptc.x - ptd.x)) in
  Printf.printf "%f %f" m1 m2;
  if m1=m2 then
    false
  else
    let p1 = float_of_int(pta.y) -. m1 *. float_of_int(pta.x) and p2 = float_of_int(ptc.y) -. m2 *. float_of_int(ptc.x) in
    let x = (p1 -. p2)/.(m2 -. m1) in
    ((max (float_of_int(pta.x)) (float_of_int(ptb.x)) ) < x && x < ( min (float_of_int(pta.x)) (float_of_int(ptb.x)) ) ) && (( max (float_of_int(ptc.x)) (float_of_int(ptd.x))) < x && x < ( min (float_of_int(ptc.x)) (float_of_int(ptd.x)) ))
*)


let conflit a1 a2 (pts_tab:point_graph array) =
  if a1 = a2 then false else   
  let ipa,ipb = a1 and ipc,ipd = a2 in
  if (ipa = ipc) || (ipa = ipd) || (ipb = ipc) || (ipb = ipd) then false else
  let pa = pts_tab.(ipa) and pb = pts_tab.(ipb) and pc = pts_tab.(ipc) and pd = pts_tab.(ipd) in
  let na = (float_of_int (pb.y - pa.y)) and da = (float_of_int (pb.x - pa.x)) and nc = (float_of_int (pd.y - pc.y)) and dc = (float_of_int (pd.x - pc.x)) in     
  let a = na /. da and c = nc /. dc in 
  let b = (float_of_int (pa.y)) -. a *. (float_of_int (pa.x)) and d = (float_of_int (pc.y)) -. c *. (float_of_int (pc.x)) in
  if (a-.c <> 0.) then
  let x = (d-.b)/.(a-.c) in
  (min (float_of_int pa.x) (float_of_int pb.x)) < x && x < (max (float_of_int pa.x) (float_of_int pb.x)) && (min (float_of_int pc.x) (float_of_int pd.x)) < x && x < (max (float_of_int pc.x) (float_of_int pd.x))
  else  
  if (d-.b) <> 0. then false else ((min pa.x pb.x) < (max pc.x pd.x) || (min pc.x pd.x) < (max pa.x pb.x)) 

let rec verif_conflit a l_edges (pts_tab:point_graph array) = match l_edges with
|[] -> false
|h::t when (conflit h a pts_tab) -> true
|h::t -> verif_conflit a t pts_tab;;  
    
(* au moins deux arrêtes *)              
let spaa l_edges pts_tab = 
    let rec aux l_edges pts_tab cpt = match l_edges with 
    |[] -> (cpt > 1)  
    |(h1,h2)::t when (((pts_tab.(h1).env) && not (pts_tab.(h2).env)) || (not (pts_tab.(h1).env) && (pts_tab.(h2).env))) -> aux t pts_tab (cpt+1)
    |(h1,h2)::t -> aux t pts_tab cpt
    in aux l_edges pts_tab 0;;


let turn_right (pt:point_graph) (pts_tab:point_graph array) graphe_model (pt_prec:point_graph) =
    let n = {i=999; x = pt.y - (pt_prec.y); y = (pt_prec.x) - pt.x} and u = {i=999; x = pt.x - (pt_prec.x); y = pt.y - (pt_prec.y)} in
    let rec parcours (liste_v:point_graph list) (n:point) (u:point) (max_pos:point_graph*float*float) (max_neg:point_graph*float*float) flag = match liste_v with
    |[] -> max_pos,max_neg,flag 
    |h::t -> let num_cos = (float_of_int ((h.x - pt.x) * (-u.x) + (h.y - pt.y) * (-u.y))) 
             and norm_n_2 = (float_of_int (n.x * n.x + n.y * n.y)) 
             and norm_2 = (float_of_int ((h.x - pt.x)*(h.x - pt.x) + (h.y - pt.y)*(h.y - pt.y)))
             and num_sin = (float_of_int ((h.x - pt.x) * n.x + (h.y - pt.y) * n.y)) in 
             let norm_n = (sqrt norm_n_2) and norm = (sqrt norm_2) in
             let cos = (num_cos /. (norm_n *. norm)) and sin = (num_sin /. (norm_n *. norm))  in 
             if (sin > 0.) then
                let _,cos_max,_ = max_pos in 
                if (1. -. cos) < (1. -. cos_max) then (parcours t n u (h,cos,sin) max_neg true)
                else (parcours t n u max_pos max_neg true)
             else if ((sin < 0.) && (not flag)) then
                let _,cos_max,_ = max_neg in 
                if (cos +. 1.) < (cos_max +. 1.) then (parcours t n u max_pos (h,cos,sin) flag)
                else (parcours t n u max_pos max_neg flag)
             else (parcours t n u max_pos max_neg flag)       
    in 
    let max_pos,max_neg,flag = parcours graphe_model.(pt.i) n u (pt_prec,(-1.),0.) (pt,1.,0.) false 
    in
    if flag then let pt_res,_,_ = max_pos in pt_res else let pt_res,_,_ = max_neg in pt_res;;

let search_polygon (graphe_model:point_graph list array) (pts_tab:point_graph array) n = 
    let list_poly = (ref []) and cas = ref 0 in 
    for i = 0 to (n-1) do
        if !cas = 0 then begin
        let head = List.hd (graphe_model.(i)) in 
        let pt = (ref head) 
        and pt_base = pts_tab.(i) in 
        let pt_prec = (ref pt_base) 
        and polygon = (ref [(!pt)]) in
        let pt_sauv = (ref head)
        and flag = (ref (( ((!pt).env) && (pt_base.env) ))) in  
        while ((!pt).i <> pt_base.i) do
            pt := (turn_right (!pt) pts_tab graphe_model (!pt_prec));
            pt_prec := (!(pt_sauv));
            polygon := ((!pt)::(!polygon));
            pt_sauv := !pt;
            flag := ((!flag) && ((!pt).env));
        done;
        if (not (!flag)) then list_poly := ((!polygon)::(!list_poly));
        cas := 1
        end;
        
        if (!cas = 1) then begin
        let head = List.hd (graphe_model.(i)) in   
        let pt_prec = (ref head) 
        and pt_base = pts_tab.(i) in 
        let pt = (ref pt_base) in 
        let polygon = (ref [(!pt)]) 
        and pt_sauv = (ref pt_base)
        and flag = (ref (( ((!pt).env) && (pt_base.env) ))) in
        while ((!pt).i <> head.i) do
            pt := (turn_right (!pt) pts_tab graphe_model (!pt_prec));
            pt_prec := (!(pt_sauv));
            polygon := ((!pt)::(!polygon));
            pt_sauv := !pt;
            flag := ((!flag) && ((!pt).env))
        done;
        if (not (!flag)) then list_poly := ((!polygon)::(!list_poly));
        cas := 0
        end
        (* On ne supprime volontairement pas les doublons car complexité de is_convex  en nlogn et complexite pour supprimer doublon en n**2*)
    done;
        !list_poly;;

 
let is_convex = fun (poly:point_graph list)->
    let zcrossproduct = fun (p1:point_graph) (p2:point_graph) (p3:point_graph) ->
        let dx1 = p3.x - p2.x and dy1 = p3.y - p2.y in
        let dx2 = p1.x - p2.x and dy2 = p1.y - p2.y in
        (dx1 * dy2 - dy1 * dx2) 
    in
    if (List.length poly) < 4 then true
    else 
        let p1 = (List.hd poly) and p2 = (List.hd (List.tl poly)) and p3 = (List.hd (List.tl (List.tl poly))) in
        let sign = ((zcrossproduct p1 p2 p3)>0) in
        let rec is_convex_rec = fun poly ->
            match poly with
                [] -> true
                | pn::[] -> (sign = ((zcrossproduct pn p1 p2)>0)) 

                | pn1::pn::[] ->    if ((sign != ((zcrossproduct pn1 pn p1)>0)) ) then false
                                    else is_convex_rec ([pn])

                | p1::p2::p3::ps -> if ((sign != ((zcrossproduct p1 p2 p3)>0)) ) then false
                                    else is_convex_rec (p2::p3::ps)
        in is_convex_rec poly;;

let all_convex (graphe_model:point_graph list array) (pts_tab:point_graph array) =
    let n = (Array.length graphe_model) in 
    let list_poly = (search_polygon graphe_model pts_tab n) in
    let rec parcours (list_poly:point_graph list list) = match list_poly with 
    |[] -> true
    |h::t when (not (is_convex h)) -> false
    |h::t -> parcours t 
    in parcours list_poly;;

let one_fct_to_rule_them_all l_edges (pts_list:point_graph list) new_a pts_env= 
  let pts_tab = Array.of_list pts_list in
    let graphe_model = (modelise_graph l_edges pts_tab) in 
    if (List.length pts_env) = (Array.length pts_tab) then
      1
    else
      if (verif_conflit new_a l_edges pts_tab) then 2
      else 
          if not (spaa l_edges pts_tab) then 0
          else 
              if (dfs graphe_model) then
                  if (all_convex graphe_model pts_tab) then 1
                  else 0
              else 0;;
         

let pts_env = [
    {i=0; x=547; y=882;env=true};  
    {i=5; x=877; y=928;env=true}; 
    {i=6; x=934; y=879;env=true}; 
    {i=7; x=555; y=682;env=true}; 
    {i=8; x=846; y=91;env=true}];;
 
let points = [|
    {i=0; x=547; y=882;env=true}; 
    {i=1; x=746; y=461;env=false}; 
    {i=2; x=556; y=825;env=false}; 
    {i=3; x=786; y=862;env=false}; 
    {i=4; x=802; y=530;env=false}; 
    {i=5; x=877; y=928;env=true}; 
    {i=6; x=934; y=879;env=true}; 
    {i=7; x=555; y=682;env=true}; 
    {i=8; x=846; y=91;env=true}|];;

let pts_list = [
    {i=0; x=547; y=882;env=true}; 
    {i=1; x=746; y=461;env=false}; 
    {i=2; x=556; y=825;env=false}; 
    {i=3; x=786; y=862;env=false}; 
    {i=4; x=802; y=530;env=false}; 
    {i=5; x=877; y=928;env=true}; 
    {i=6; x=934; y=879;env=true}; 
    {i=7; x=555; y=682;env=true}; 
    {i=8; x=846; y=91;env=true}];;

let edges = [(0, 2) ; (0,7) ; (1, 2) ; (1, 4); (1, 8); (7,8); (2, 7); (5,0) ; (3, 4); (8,6) ; (3, 5); (3, 8); (6,5) ; (4, 8)];;
let edges2 = [(0, 2) ; (1, 2) ; (1, 4); (1, 8); (2, 7); (3, 4); (3, 5); (3, 8); (4, 8); (0,7) ; (7,8); (8,6) ; (6,5) ; (5,0) ];;
  
           
            
                 
        
               
     


    
     
                       
            

(*preparation tests

let a = {i=0;x=(-2396);y=(-5284);env=true};;
let b = {i=1;x=2656;y=2938;env=true};;
let c = {i=2;x=4120;y=2278;env=false};;
let d = {i=3;x=4342;y=102;env=false};;
let e = {i=4;x=4384;y=2988;env=false};;
let f = {i=5;x=5136;y=2280;env=false};;
let g = {i=6;x=6634;y=5416;env=true}
let h = {i=7;x=8598;y=2632;env=false};;
let i = {i=8;x=8898;y=4170;env=true};;
let j = {i=9;x=11738;y=1550;env=true};;

let pts_tab = [|a;b;c;d;e;f;g;h;i;j|];;

let a = {i=0;x=(-2396);y=(-5284)};;
let b = {i=1;x=2656;y=2938};;
let c = {i=2;x=4120;y=2278};;
let d = {i=3;x=4342;y=102};;
let e = {i=4;x=4384;y=2988};;
let f = {i=5;x=5136;y=2280};;
let g = {i=6;x=6634;y=5416}
let h = {i=7;x=8598;y=2632};;
let i = {i=8;x=8898;y=4170};;
let j = {i=9;x=11738;y=1550};;  

let l_edges = [(0,9);(0,3);(2,3);(2,5);(2,4);(4,5);(5,7);(3,7);(7,9);(7,6);(4,6);(4,1);(9,8);(8,6);(6,1);(1,0)];;
let pts_tab_2 = [|a;b;c;d;e;f;g;h;i;j|];; 

(*let rec construct_for_test l_edges pts_tab =
    let pts_list = list_of_array pts_tab in 
    let rec aux graphe l_edges pts_list = match l_edges with 
    |[] -> graphe
    |h::t -> aux {points = pts_list; edges = h::(graphe.edges)} t pts_list
    in aux {points = pts_list; edges = []} l_edges pts_list;; *)  
      
let a1 = (0,6);;
let a2 = (0,9);;
*)

