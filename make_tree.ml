open Types

type arbre = Vide | Node of edge * arbre list | End

let rec is_in_env_conv pt pts_env = match pts_env with
  |[] -> false
  |h::q when h = pt -> true
  |h::q -> is_in_env_conv pt q

let create_all_edges pts_graph pts_env= 
  let rec ptconsecutif (p1:point_graph) (p2:point_graph) l (prems) = match l with
  |h::[] -> (p1.i = h.i && p2.i = prems.i) || (p1.i = prems.i && p2.i = h.i)
  |h1::h2::q when ((p1.i,p2.i) = (h1.i, h2.i)) || ((p1.i,p2.i) = (h2.i, h1.i)) -> true
  |h1::h2::q -> ptconsecutif p1 p2 (h2::q) prems
  in
  let rec aux pts res = 
    let rec create_edges pt pt_list res2 = match pt_list with
      |[] -> res2
      |h::q when (not ((h.env && pt.env) && (ptconsecutif h pt pts_env (List.hd pts_env)))) -> create_edges pt q ((pt.i, h.i)::res2)
      |h::q -> create_edges pt q res2
    in match pts with
    |[] -> res
    |h::q -> aux q (create_edges h q res)
  in Array.of_list (aux pts_graph [])

let conditions list_pere pts_graph arrete pts_env= Test_graph.one_fct_to_rule_them_all list_pere pts_graph arrete pts_env;;

let pt_to_ptgraph pts_env (pt:point) = {i=pt.i; x=pt.x; y=pt.y; env = (is_in_env_conv pt pts_env)}

let gen_arbre graph pts_env pivot= 
  Printf.printf "generating tree ...\n";
  let points_graph = List.map (pt_to_ptgraph pts_env) graph.points in
  let all_edges = create_all_edges points_graph pts_env in
  let rec aux edges k list_pere h minh=
    let n = (Array.length edges - 1) in
    let res = ref [] in
    if k > n || h >= (!minh) then
      res := [Vide]
    else
      for i=n downto k do
        if h < (!minh) then
        begin
          let e = edges.(i) in
          (*edges.(i) <- edges.(k);
          edges.(k) <- e;*)
          (*List.iter (fun (a,b) -> Printf.printf "(%d, %d) " a b) (e::list_pere); Printf.printf "\n";*)
          match (conditions (e::list_pere) points_graph e pts_env) with
          |j when j=0 -> res := (Node(e, (aux (Array.copy edges) (i+1) (e::list_pere) (h+1) minh)))::(!res)
          |j when j=1 -> res := (Node(e, [End]))::(!res); minh := h
          |_ (*when j=2*)-> if ((!res) = []) && (i=k) then res := [Vide]
        end
      done;
    !res
  in 
aux all_edges 0 (Enveloppe.enveloppe_convexe_2 pts_env pivot) 0 (ref max_int);;

let gen_arbre_g graph pts_env pivot = 
  Printf.printf "generating tree ...\n";
  let points_graph = List.map (pt_to_ptgraph pts_env) graph.points in
  let all_edges = create_all_edges points_graph pts_env in
  let rec aux edges k list_pere h minh=
    let n = (Array.length edges - 1) in
    let res = ref [] in
    if k > n || h >= (!minh) then
      res := [Vide]
    else
      for i=k to n do
        if h < (!minh) then
        begin
          let e = edges.(i) in
          (*edges.(i) <- edges.(k);
          edges.(k) <- e;*)
          (*List.iter (fun (a,b) -> Printf.printf "(%d, %d) " a b) (e::list_pere); Printf.printf "\n";*)
          match (conditions (e::list_pere) points_graph e pts_env) with
          |j when j=0 -> res := (Node(e, (aux (Array.copy edges) (i+1) (e::list_pere) (h+1) minh)))::(!res)
          |j when j=1 -> res := (Node(e, [End]))::(!res); minh := h
          |_ (*when j=2*)-> if ((!res) = []) && (i=n) then res := [Vide]
        end
      done;
    !res
  in 
aux all_edges 0 (Enveloppe.enveloppe_convexe_2 pts_env pivot) 0 (ref max_int);;

let get_sol tree_l = 
  let rec parcours queue = 
    let e = Queue.take_opt queue in
    match e with
    |None -> None
    |Some ([Vide], res) -> parcours queue
    |Some ([End], res) -> Some res
    |Some (tree_list, res) -> List.iter (fun (Node (e,l)) -> Queue.push (l, e::res) queue) tree_list; parcours queue
  in 
  let q = Queue.create () in
  List.iter (fun (Node (e,l)) -> Queue.push (l, [e]) q) tree_l;
  parcours q