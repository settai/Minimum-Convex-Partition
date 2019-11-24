open Scanner

let plot_point = fun pt ->
    Graphics.fill_circle pt.x pt.y 4


let () = 
    Graphics.open_graph "";
    let points = (Scanner.input_points "points.json") in
    List.map plot_point points;
    ignore(Graphics.wait_next_event [Key_pressed]);
    Graphics.close_graph ();
