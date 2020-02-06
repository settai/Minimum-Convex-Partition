open Types
open Test_graph
open Make_tree

let () =
  Random.self_init ();
  Graphics.open_graph "";
  ignore(Graphics.wait_next_event [Key_pressed]);

  let n = 9 in

  let p_tab = Array.make n {i=0; x=0; y=0} in
  for i=0 to (n-1) do
    p_tab.(i) <- {i=i; x=(Random.int 1000); y=(Random.int 1000)}
  done;
  let points = Array.to_list p_tab in

  Printf.printf "[";
  List.iter (fun {i=i; x=x; y=y} -> Printf.printf "{i=%d; x=%d; y=%d}; " i x y) points;
  Printf.printf "]\n";

  let vide = {points=points; edges=[]} in
  Graph.plot_graph vide;
  ignore(Graphics.wait_next_event [Key_pressed]);

  let env, pivot = Enveloppe.enveloppe_convexe_graham vide in
  let env_edges = Enveloppe.enveloppe_convexe_2 env pivot in
  let pts_graph = List.map (pt_to_ptgraph env) points in
  Graph.plot_graph {points=points; edges = env_edges};
  ignore(Graphics.wait_next_event [Key_pressed]);

  
  let arbre_d = gen_arbre vide env pivot in
  let (Some sol_d) = get_sol arbre_d in
  Graph.plot_graph ({points=points; edges=sol_d});
  Printf.printf "droit: %d\n" (one_fct_to_rule_them_all (sol_d@env_edges) pts_graph (List.hd (sol_d@env_edges)) env);
  Printf.printf "["; List.iter (fun (i,j) -> Printf.printf "(%d, %d); " i j) sol_d; Printf.printf "]\n";
  ignore(Graphics.wait_next_event [Key_pressed]);

  (*
  let arbre_g = gen_arbre_g vide env pivot in
  let (Some sol_g) = get_sol arbre_g in
  Graph.plot_graph {points=points; edges=env_edges};
  Graph.plot_graph ({points = vide.points; edges=sol_g});
  Printf.printf "gauche: %d\n" (one_fct_to_rule_them_all (sol_g@env_edges) pts_graph (List.hd (sol_g@env_edges)) env);
  Printf.printf "["; List.iter (fun (i,j) -> Printf.printf "(%d, %d); " i j) sol_g; Printf.printf "]\n";
  ignore(Graphics.wait_next_event [Key_pressed]);
  Graph.clear ();*)

  

  Graphics.close_graph ();
