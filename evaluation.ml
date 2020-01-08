let evaluer_poly = fun liste ->
    let rec aux = fun l acc -> 
        match l with 
        []-> acc
        |p::q -> aux q (acc+1)
     in aux liste 0;;

let random_tableau = fun tab ->
    let n = Array.length tab in
    Random.self_init();
    let rec aux = fun t acc borne_max ->
        match acc with 
        0 -> t
        |_ -> let a = Random.int borne_max in
                let b = Random.int borne_max in
                let c = t.(a) in
                let d = t.(b) in
                t.(a) <- d;
                t.(b) <- c;
                aux t (acc-1) borne_max
    in aux tab (n) (n)

let random_liste = fun liste ->
    let tableau=Array.of_list liste in
    let tab = random_tableau tableau in
    let liste_finale=Array.to_list tab in
    (*List.map (fun (x,y) -> Printf.printf "( %d , %d ); " x y) liste_finale;*)
    liste_finale;;
