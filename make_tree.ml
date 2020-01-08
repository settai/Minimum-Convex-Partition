open Types

type arbre = Vide | Node of edge * arbre list | End

type point_graph = {x: int; y: int; indice: int; env: bool}

let rec is_in_env_conv pt pts_env = match pts_env with
  |[] -> false
  |h::q when h = pt -> true
  |h::q -> is_in_env_conv pt q

let create_all_edges pts_graph = 
  let rec aux pts res = 
    let rec create_edges pt pt_list res2 = match pt_list with
      |[] -> res2
      |h::q when (not (h.env && pt.env)) -> create_edges pt q ((pt.indice, h.indice)::res2)
      |h::q -> create_edges pt q res2
    in match pts with
    |[] -> res
    |h::q -> aux q (create_edges h q res)
  in Array.of_list (aux pts_graph [])

let conditions pts_graph pts_env list_pere = 0;;

let pt_to_ptgraph pts_env (pt:point) = {indice=pt.i; x=pt.x; y=pt.y; env = (is_in_env_conv pt pts_env)}

let gen_arbre graph pts_env= 
  let points_graph = List.map (pt_to_ptgraph pts_env) graph.points in
  let all_edges = create_all_edges points_graph in
  let rec aux edges k list_pere =
    let n = (Array.length edges - 1) in
    let res = ref [] in
    if k >= n then
      res := [Vide];
    for i=k to n do
      let e = edges.(i) in
      edges.(i) <- edges.(k);
      edges.(k) <- e;
      match (conditions points_graph pts_env (e::list_pere)) with
      |j when j=0 -> res := (Node(e, (aux (Array.copy edges) (i+1) (e::list_pere))))::(!res)
      |j when j=1 -> res := (Node(e, [End]))::(!res)
      |_ (*when j=2*)-> res := Vide::(!res)
    done;
    !res
  in 
  aux all_edges 0 [];;

let get_sol tree_l = 
  let rec parcours queue = 
    let e = Queue.take_opt queue in
    match e with
    |None -> None
    |Some ([Vide], res) -> None
    |Some ([End], res) -> Some res
    |Some (tree_list, res) -> List.iter (fun (Node (e,l)) -> Queue.push (l, e::res) queue) tree_list; parcours queue
  in 
  let q = Queue.create () in
  List.iter (fun (Node (e,l)) -> Queue.push (l, [e]) q) tree_l;
  parcours q