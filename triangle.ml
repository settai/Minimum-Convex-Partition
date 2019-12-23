open Types
exception Poly_found of polygone

open Enveloppe_convexe

exception Error;; 

let vecteur = fun a b ->
    (b.x-a.x,b.y-a.y) ;;

let determinant = fun (ax,ay) (bx,by) ->
    ax * by - ay * bx ;;

let saillant = fun p q r -> 
    0 < (determinant (vecteur p q) (vecteur q r)) ;;

let rentrant = fun p q r ->
    0 > (determinant (vecteur p q) (vecteur q r)) ;;

let alignes = fun p q r ->
    0 = (determinant (vecteur p q) (vecteur q r)) ;;


let carre =fun x -> x * x ;;

let d2 = fun (ax,ay) (bx,by) -> carre(ax - bx) + carre(ay - by) ;;

let cercle_circonscrit = fun (ax,ay) (bx,by) (cx,cy) ->
    let a = d2 (ax,ay) (0,0)
    and b = d2 (bx,by) (0,0)
    and c = d2 (cx,cy) (0,0)
    and q = 2 * (ax * (by-cy) + bx * (cy-ay) + cx * (ay-by))
    in
    (
    (* abscisse du centre *)
    (a *(by-cy) + b * (cy-ay) + c * (ay-by))/q ,
    (* ordonnee du centre *)
    -(a * (bx-cx) + b * (cx-ax) + c * (ax-bx))/q),
    (* rayon du cercle *)
    sqrt(float_of_int ((d2 (ax,ay) (bx,by))*(d2 (bx,by) (cx,cy))*(d2 (cx,cy) (ax,ay))))/. abs_float( float_of_int q) ;;


let sens_direct = fun a b c ->
    0 < (determinant (vecteur a b) (vecteur a c)) ;;


let est_triangle = fun a b c ->
    not (alignes a b c) ;;


(*Prends en entree un triangle et un point et renvoie un booleen pour indiquer si le point est dans le triangle ou non*)
let est_dans_triangle [a;b;c] m =
    if ((((determinant (vecteur m a) (vecteur m b)) * (determinant (vecteur m b) (vecteur m c))) >= 0) && (((determinant (vecteur m b) (vecteur m c)) * (determinant (vecteur m c) (vecteur m a))) >= 0))then  true else false ;;

let strict_dans_triangle  [a;b;c] m =
    if (((determinant (vecteur m a) (vecteur m b)) * (determinant (vecteur m b) (vecteur m c)) > 0) && ((determinant (vecteur m b) (vecteur m c)) * (determinant (vecteur m c) (vecteur m a)) >0)&& ((determinant (vecteur m c) (vecteur m a)) * (determinant (vecteur m a) (vecteur m b)) >= 0)) then true else false ;;
    

(*Prends en entrée une liste de points et renvoie les points de l'enveloppe sous forme de l'enveloppe*) 
let rec obtention_enveloppe = fun ens_pts ->
    let graphe = {points = ens_pts; edges= [] } in
    let (liste,pivot) = enveloppe_convexe_graham graphe in
    liste;;



(* Prends en entrée 3 triangle et une liste de points à l'intérieur de ces triangles et renvoie 3 listes de point à l'intérieur de chaque triangle*)
let split = fun triangle1 triangle2 triangle3 liste ->
    let rec split_rec = fun t1 t2 t3 l l1 l2 l3 ->
        match l with
        [] -> (l1 , l2 , l3)
        |a::b -> if est_dans_triangle t1 a then  split_rec t1 t2 t3 b (a::l1) l2 l3 else if est_dans_triangle t2 a then  split_rec t1 t2 t3 b l1 (a::l2) l3 else split_rec t1 t2 t3 b l1 l2 (a::l3)
        in split_rec triangle1 triangle2 triangle3 liste [] [] [];;



(* Prends en entrée une liste de triangle [a,b,c,...] et une liste de points à l'intérieur de ces triangles et renvoie une liste de liste de point à l'intérieur de chque triangle*)
let split2 = fun liste_triangle liste ->
    let liste_ptsint = [] in
    let rec aux = fun triangle_liste l l_copie pt_liste acc-> 
        match l,triangle_liste with 
        |_,[] -> pt_liste
        |[],h::t-> let pt_liste=(acc::pt_liste) in
                    let acc = [] in 
                    aux t l_copie l_copie pt_liste acc
        |x::y,h::t -> if (est_dans_triangle h x) then aux (h::t) y l_copie pt_liste (x::acc) else aux (h::t) y l_copie pt_liste acc 
     in aux liste_triangle liste liste liste_ptsint [];;

                                                    


(* Prends en entrée un triangle et un point et renvoie tous les triangles entre ce points et les
points du polygone*)
let split_triangle [a; b; c] p = 
    [[a; b; p]; [a;p; c]; [p; b; c]];;


(* Prends en entrée un polygone et un point et renvoie tous les triangles entre ce points et les
points du polygone*)
let split_liste_in_triangle poly pt = 
    let rec aux l copy_l p res =
        match l with 
        [] -> List.rev res
        |a::b::t -> aux (b::t) copy_l p ([a;b;p]::res)
        |b::[] -> match copy_l with 
                    a::t -> aux [] copy_l p ([a;b;p]::res)
                    |_ -> raise Error
     in aux poly poly pt [];;
        

let tri_to_edge triangles = 
    let edges = ref [] in 
    List.iter (fun [a;b;c] -> 
        if (a.i >= 0) && (b.i >= 0) then
            edges := (a.i, b.i)::(!edges);
        if (b.i >= 0) && (c.i >= 0) then
            edges := (b.i, c.i)::(!edges);
        if (c.i >= 0) && (a.i >= 0) then
            edges := (c.i, a.i)::(!edges);
        ) triangles;
    !edges;;

let print_triangle (a, b, c) = Printf.printf "(%d, %d, %d)\n" a.i b.i c.i;;

(* Prend en entrée un elt et une liste et renvoie un booleen signifiant que l elt est dans la liste*)
let rec in_list = fun elt liste1 ->
    match liste1 with
    [] -> false
    |h::t -> if (elt=h) then true else in_list elt t;;


let virer_doublon = fun liste1  liste2 ->
    let rec aux = fun l1 l2 res ->
        match l1 with 
        [] -> res
        |h::t -> if (in_list h l2) then aux t l2 res else aux t l2 (h::res)
    in 
    aux liste1  liste2 [];;


let rec deuxieme_partie_triangulation ps triangle = 
        match ps with
        |[] -> [triangle]
        |p::q -> let [t1;t2;t3] = split_triangle triangle p in
                    let ps1, ps2, ps3 = split t1 t2 t3 q in
                    (deuxieme_partie_triangulation ps1 t1)@(deuxieme_partie_triangulation ps2 t2)@(deuxieme_partie_triangulation ps3 t3);;


let det_pt_int = fun triangle file_pts ->
            let rec aux file pint tri =
                            match file with
                            [] -> pint 
                            |h::t -> if est_dans_triangle tri h then aux t (h::pint) tri else  aux t pint tri
               in aux file_pts [] triangle;;

let rec fin_triangulation = fun li1 li2 res ->
        match li1 with 
        [] -> res
        |x::y -> fin_triangulation y li2 ((deuxieme_partie_triangulation (det_pt_int x li2) x)@res);;           

let triangulation pts = 
    let enveloppe = obtention_enveloppe pts in
    let pts_filtre = virer_doublon pts enveloppe in
    match pts_filtre with
        [] -> [enveloppe]
        |p::q -> fin_triangulation (split_liste_in_triangle enveloppe p) q [] ;;

(* is_convex : convertir points list en int list list*)
let points_to_poly = fun points_list ->
    List.map (fun points -> List.map (fun point -> point.i) points) points_list

(* delaunay_to_edges : adaptation de la triagulation de delaunay implemanter dans la bible pour notre structure*)
let delaunay_to_edges = fun points ->
    let triangulation = Delaunay.Int.triangulate (Array.of_list points) in
    let to_edges = fun arcs_array ->
        let arcs = Array.to_list arcs_array in
        let list_vert = List.map ( fun arc -> match arc.Delaunay.Int.vert, arc.next.vert with
            			Delaunay.Int.Point p1 , Delaunay.Int.Point p2  -> (p1,p2)
            			| Delaunay.Int.Infinity,_ | _,Delaunay.Int.Infinity -> (-1,-1)) arcs
        in
        let rec remove_double = fun d_edges edges ->
            match d_edges with 
            [] -> edges
            | (p1,p2)::es when not (List.exists (fun (q1,q2) -> q1=p2 && q2=p1) edges) -> remove_double es ((p1,p2)::edges)
            | _::es -> remove_double es edges
        in
        let rec to_edges_rec = fun list_vert edges_out ->
			match list_vert with
				[] -> edges_out
				| (-1,-1)::es -> to_edges_rec es edges_out
				| e::es -> to_edges_rec es (e::edges_out)
		in
		remove_double (to_edges_rec list_vert []) []
    in
    to_edges triangulation.arcs

(* is_convex :  test si un polygone est convex, avec la liste de tous les cordonnes des points du polygone*)
let is_convex = fun points poly->
    let zcrossproduct = fun p1 p2 p3 ->
        let pt1 = (Graph.find_point points p1) and pt2 = (Graph.find_point points p2) and pt3 = (Graph.find_point points p3) in
        let dx1 = pt3.x - pt2.x and dy1 = pt3.y - pt2.y in
        let dx2 = pt1.x - pt2.x and dy2 = pt1.y - pt2.y in
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
        in is_convex_rec poly

(* poly_fusion : fusionne deux polygone ayant un trait en commun *)
let poly_fusion = fun polygone1 polygone2 ->
    let rotate_poly = fun poly ->
        let last = List.hd (List.rev poly) in
        let rec init = function
        | [] -> []
        | [x] -> []
        | x::xs -> x::(init xs)
        in last::(init poly) 
    in
    let n1 = List.length polygone1 and n2 = List.length polygone2 in
    let poly1 = ref polygone1 and poly2 = ref polygone2 in
    let poly_found = fun () ->
        for count1 = 1 to n1 do
            begin
            match !poly1 with
                p11::p12::ps -> 
                    for count2 = 1 to n2 do
                        match !poly2 with
                            p21::p22::ps -> 
                                if p21 = p11 && p22 = p12 then 
                                    let ph1 = List.hd !poly1 and pt1 = List.tl !poly1 and polytail = List.rev (List.tl (List.tl !poly2)) in raise (Poly_found (pt1@[ph1]@polytail))
                                else if p21 = p12 && p22 = p11 then 
                                    let ph1 = List.hd !poly1 and pt1 = List.tl !poly1 and polytail = List.tl (List.tl !poly2) in raise (Poly_found (pt1@[ph1]@polytail))
                                else poly2 := rotate_poly !poly2
                            | _ -> failwith "Invalid polygone"
                    done
                | _ -> failwith "Invalid polygone"
            end;
            poly1 := rotate_poly !poly1   
        done;
        failwith "Fusion failed"
    in 
    try (poly_found ())
    with  Poly_found poly -> poly

(* delaunay_to_polygones : la triangulation de delaunay ayant comme sortie une liste de polygone qui represente la liste des triangles *)
let delaunay_to_polygones = fun points ->
    let triangulation = Delaunay.Int.triangulate (Array.of_list points) in
    let to_polygones = fun arcs_array ->
        let arcs = Array.to_list arcs_array in
        let list_polygones = List.map ( fun arc -> match arc.Delaunay.Int.vert, arc.next.vert, arc.next.next.vert with
            			Delaunay.Int.Point p1 , Delaunay.Int.Point p2 , Delaunay.Int.Point p3 -> [p1;p2;p3]
            			| _ -> []) arcs
        in
        let rec to_polygones_rec = fun list_polygones polygones_out ->
			match list_polygones with
				[] -> polygones_out
				| []::ps -> to_polygones_rec ps polygones_out
				| p::ps -> to_polygones_rec ps (p::polygones_out)
		in
        let rec remove_double = fun d_polygones polygones ->
            match d_polygones with 
            [] -> polygones
            | p::ps ->  begin
                        match p with         (* Test if the triangle already exist*)
                        p1::p2::p3::[] ->      if (List.exists (fun (q1::q2::q3::[]) -> 
                                                (p1=q1 || p2=q1 || p3 = q1) && 
                                                (p1=q2 || p2=q2 || p3 = q2) && 
                                                (p1=q3 || p2=q3 || p3 = q3)) polygones)
                                            then remove_double ps polygones
                                            else remove_double ps (p::polygones)
                        | _ -> failwith "error delaunay to polygone"
                        end          
            | _::ps -> remove_double ps polygones
        in  
        (* to_polygones_rec list_polygones [] *)
		remove_double (to_polygones_rec list_polygones []) []
    in
    to_polygones triangulation.arcs

(* exist_in_poly : test si le trait (a,b) exist dans un polygone*)
let exists_in_poly = fun a b poly->
    (List.exists (fun x -> x=a) poly) && (List.exists (fun x -> x=b) poly)

(* remove_poly : supprime un polygone de la liste des polygones*)
let remove_poly = fun poly list_poly ->
    let rec remove_poly_rec = fun list_poly new_list ->
        match list_poly with
        [] -> new_list
        | p::ps when p=poly -> remove_poly_rec ps new_list
        | p::ps -> remove_poly_rec ps (p::new_list)
    in remove_poly_rec list_poly []

(* poly_to_edges : transforme un polygone à une liste des traits *)
let poly_to_edges = fun polygone ->
    let rec poly_to_edges_rec = fun poly edges->
        match poly with 
        [] -> []
        | pn::[] -> (pn,(List.hd polygone))::edges
        | p1::p2::ps -> poly_to_edges_rec (p2::ps) ((p1,p2)::edges)
    in poly_to_edges_rec polygone []

(* poly_reduction : parcours la lists des polygones et pour chaque edge commun entre deux polygone il essaie si on peut l'enlever tout en gardant la convexité, la liste des points sert pour retrouver les cordonnés des references*)
let poly_reduction = fun list_polygones points->
    let list_poly = ref list_polygones in
    let list_edges = List.concat (List.map poly_to_edges !list_poly) in
    let rec poly_reduction_rec = fun edges ->
        match edges with 
        [] -> List.concat (List.map poly_to_edges !list_poly)
        | (a,b)::es ->  let p = List.filter (exists_in_poly a b) !list_poly in
                        begin 
                        match p with
                        p1::p2::_ ->
                            let poly = poly_fusion p1 p2 in
                            (* Printf.printf "%B" (is_convex points poly); *)
                            if (is_convex points poly) then list_poly := (poly::(remove_poly p2 (remove_poly p1 !list_poly)));
                            poly_reduction_rec es
                        | _ -> poly_reduction_rec es
                        end
    in poly_reduction_rec list_edges
