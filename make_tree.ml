open Types

type arbre = Vide | Node of edge * arbre list

type point_graph = {x: int; y: int; i: int; env: bool}

let rec is_in_env_conv pt pts_env = match pts_env with
  |[] -> false
  |h::q when h = pt -> true
  |h::q -> is_in_env_conv pt q

let create_all_edges pts_graph = 
  let rec aux pts res = 
    let rec create_edges pt pt_list res2 = match pt_list with
      |[] -> res2
      |h::q when not (h.env && pt.env) -> create_edges pt q ((pt.i, h.i)::res2)
      |h::q -> create_edges pt q res
    in match pts with
    |[] -> res
    |h::q -> aux q (create_edges h q res)
  in Array.of_list (aux pts_graph [])

let conditions pts_graph pts_env list_pere = 0;;

let gen_arbre graph pts_env= 
  let points_graph = List.map (fun (pt:point) -> {x=pt.x; y=pt.y; i=pt.i; env = (is_in_env_conv pt pts_env)}) graph.points in
  let all_edges = create_all_edges points_graph in
  let rec aux edges k list_pere =
    let res = ref [] in
    for i=k to (Array.length edges) do
      let e = edges.(i) in
      edges.(i) = edges.(k);
      edges.(k) = e;
      match (conditions points_graph pts_env (e::list_pere)) with
      |j when j=0 -> res := (Node(e, (aux edges (k+1) (e::list_pere))))::(!res)
      |j when j=1 -> res := (Node(e, [Vide]))::(!res)
      |_ (*when j=2*)-> ()
    done;
    !res
  in 
  aux all_edges 0 []
  
