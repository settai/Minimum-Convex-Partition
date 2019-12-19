type arbre = Vide | Node of edge * arbre list

type point_graph = {x: int; y: int; env: bool}

let rec is_in_env_conv pt pts_env = match pts_env with
  |[] -> false
  |h::q when h = pt -> true
  |h::q -> is_in_env_conv pt q

let create_all_edges pts_graph = 
  let rec aux pts res = 
    let rec create_edges pt pt_list res2 = match pt_list with
      |[] -> res2
      |h::q when !(h.env && pt.env) -> create_edges pt q ((pt.i, h.i)::res2)
      |h::q -> create_edges pt q res
    in match pts with
    |[] -> aux
    |h::q -> create_edges h q res
  in Array.of_list (aux pts_graph [])

let gen_arbre graph pts_env= 
  let points_graph = List.map (fun pt -> {x=pt.x; y=pt.y; env = (is_in_env_conv pt pts_env)}) graph.points in
  let all_edges = create_all_edges points_graph in
  let rec aux edges k list_pere =
    for i=k to Array.length do
      let e = edges.(i) in
      edges.(i) = edges.(k);
      edges.(k) = e;
      match Conditions points_graph pts_env (e::list_pere) with
      |j when j=0 -> Node(e, (aux edges (k+1) (e::list_pere)))
      |j when j=1 -> Node(e, [Vide])
      |_ -> [Vide]
    done;
  in 
  aux all_edges 0 []
  
