open Types

let () =     
    let points = (Scanner.input_points Sys.argv.(1)) in
	Graphics.open_graph "";
    ignore(Graphics.wait_next_event [Key_pressed]);
    Graph.plot_graph {points=points; edges=[]};
    ignore(Graphics.wait_next_event [Key_pressed]);
    
    let edges = Triangle.delaunay points in
    let test = {points=points; edges=edges} in
    Graph.plot_graph test;
    ignore(Graphics.wait_next_event [Key_pressed]);
    Graphics.close_graph ();

(*{i= (-1); x=0; y=25026}::{i= (-2); x= (-25026); y= (-25026)}::{i= (-3); x=25026; y= (-25026)}*)
