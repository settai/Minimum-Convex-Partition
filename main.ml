open Types
open Make_tree

let () = 
  Graphics.open_graph "";
  ignore(Graphics.wait_next_event [Key_pressed]);
  
  let points = (*(Scanner.input_points "points.json")*) [{i=0; x=0; y=0}; {i=1; x=1; y=2}; {i=2; x=2; y=0}; {i=3; x=1; y=1}] in
  Graph.plot_graph {points=points; edges=[]};
  ignore(Graphics.wait_next_event [Key_pressed]);
  
  let vide = {points=points; edges=[]} in
  let env, pivot = Enveloppe.enveloppe_convexe_graham vide in
  let test = {points=points; edges=(Enveloppe.enveloppe_convexe_2 env pivot)} in
  Graph.plot_graph test;
  ignore(Graphics.wait_next_event [Key_pressed]);

  let arbre = Make_tree.gen_arbre vide env pivot in
  let (Some sol) = Make_tree.get_sol arbre in
  Graph.plot_graph ({points = vide.points; edges=sol});
  ignore(Graphics.wait_next_event [Key_pressed]);
  Graphics.close_graph ();
