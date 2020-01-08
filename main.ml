open Types

let () =     
    let points = (Scanner.input_points Sys.argv.(1)) in
	Graphics.open_graph "";
    ignore(Graphics.wait_next_event [Key_pressed]);
    Graph.plot_graph {points=points; edges=[]};
    ignore(Graphics.wait_next_event [Key_pressed]);
    
    let arrete = Triangle.delaunay_to_edges points in
    let poly = Triangle.delaunay_to_polygones points in
    let reference = Evaluation.evaluer_poly poly in
    let simplifie =  (Deterministe.optim_locale poly arrete points) in
    let edges = List.concat (List.map Triangle.poly_to_edges simplifie) in
    Printf.printf "%d  %f\n" (Evaluation.evaluer_poly simplifie) ((float_of_int (Evaluation.evaluer_poly (simplifie)))/.(float_of_int reference));
    
    (*let edges = Triangle.poly_reduction (Triangle.points_to_poly (Triangle.triangulation points)) points in*)
    
    
    (*let edges = Triangle.poly_reduction (Triangle.delaunay_to_polygones points) points in*)
    
    (*let edges = Triangle.poly_red_alea (Triangle.delaunay_to_polygones points) points in*)
    
    
    let test = {points=points; edges=edges} in
    
    Graph.plot_graph test;
    ignore(Graphics.wait_next_event [Key_pressed]);
    Graphics.close_graph ();

(*{i= (-1); x=0; y=25026}::{i= (-2); x= (-25026); y= (-25026)}::{i= (-3); x=25026; y= (-25026)}*)

