open Types

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



let rec est_dans_triangle = fun (a , b , c ) m ->
    if (((determinant (vecteur m a) (vecteur m b)) * (determinant (vecteur m b) (vecteur m c)) >= 0) && ((determinant (vecteur m b) (vecteur m c)) * (determinant (vecteur m c) (vecteur m a)) >= 0)) then true else false ;;

let rec strict_dans_triangle = fun (a , b , c ) m ->
    if (((determinant (vecteur m a) (vecteur m b)) * (determinant (vecteur m a) (vecteur m b)) > 0) && ((determinant (vecteur m b) (vecteur m c)) * (determinant (vecteur m c) (vecteur m a)) >0)) then true else false ;;
    
    
(*let rec triangle = fun k pt pd ->
    match k with 
    [] -> (pd , pd , pd)
    |a::b -> if (est_dans_triangle a pt) then begin 
                  let k=b in
                  let pas_int = a in
                  pas_int
                  end;
                  else  triangle k pt pd;;*)

let rec debut_Delauney = fun t l pa pb pc ->
    match t with 
    [] -> (pa,pb,pc)
    |a::h -> if est_dans_triangle (pa , pb , pc ) a then debut_Delauney h l pa pb pc else begin
     let pa2 = {i=pa.i; x=pa.x; y=(pa.y+1)} in
     let pb2 = {i=pb.i; x=(pb.x-1); y=(pb.y-1)} in
     let pc2 = {i=pc.i; x=(pc.x+1); y=(pc.y-1)} in
     debut_Delauney l l pa2 pb2 pc2
     end;; 


let split = fun triangle1 triangle2 triangle3 liste ->
    let rec split_rec = fun t1 t2 t3 l l1 l2 l3 ->
        match l with
        [] -> (l1 , l2 , l3)
        |a::b -> if est_dans_triangle t1 a then  split_rec t1 t2 t3 b (a::l1) l2 l3 else if est_dans_triangle t2 a then  split_rec t1 t2 t3 b l1 (a::l2) l3 else split_rec t1 t2 t3 b l1 l2 (a::l3)
        in split_rec triangle1 triangle2 triangle3 liste [] [] [];;


let split_triangle (a, b, c) p = 
    (a, b, p), (a, p, c), (p, b, c);;

let tri_to_edge triangles = 
    let edges = ref [] in 
    List.iter (fun (a, b, c) -> 
        if (a.i >= 0) && (b.i >= 0) then
            edges := (a.i, b.i)::(!edges);
        if (b.i >= 0) && (c.i >= 0) then
            edges := (b.i, c.i)::(!edges);
        if (c.i >= 0) && (a.i >= 0) then
            edges := (c.i, a.i)::(!edges);
        ) triangles;
    !edges;;

let print_triangle (a, b, c) = Printf.printf "(%d, %d, %d)\n" a.i b.i c.i;;

let triangulation pts = 
    List.iter (fun x -> Printf.printf "(%d, %d, %d)\n" x.i x.x x.y) pts;
    let pa = {i= -1 ; x = 0 ; y = 1} in 
    let pb = {i= -2 ; x = -1 ; y = -1} in
    let pc = {i= -3 ; x = 1 ; y = -1} in
    let debut = debut_Delauney pts pts pa pb pc in
    let rec aux ps triangle = 
        match ps with
        |[] -> [triangle]
        |p::q -> let t1,t2,t3 = split_triangle triangle p in
                    let ps1, ps2, ps3 = split t1 t2 t3 q in
                    print_triangle t1; print_triangle t2; print_triangle t3;
                    Printf.printf "[" ; List.iter (fun x -> Printf.printf "%d " x.i) ps1; Printf.printf "]\n";
                    Printf.printf "[" ; List.iter (fun x -> Printf.printf "%d " x.i) ps2; Printf.printf "]\n";
                    Printf.printf "[" ; List.iter (fun x -> Printf.printf "%d " x.i) ps3; Printf.printf "]\n";
                    (aux ps1 t1)@(aux ps2 t2)@(aux ps3 t3)
        in
    aux pts debut;;








