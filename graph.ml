open Types;;

let rec find_point = fun pts i ->
        match pts with
        p::ps -> if p.i = i then p
                   else find_point ps i 

let calc_pos = fun x min max size ->
        ((x-min) * (size-50) / (max-min))+25

let iter = fun f pl ->
        let rec iter_rec = fun points m ->
                match points with
                [] -> m
                | p::ps -> iter_rec ps (f m p) in
        match pl with
        [] -> 0
        | p::ps -> iter_rec ps p

let plot_point = fun (xmin,xmax) (ymin,ymax) pt ->
    let posx = calc_pos pt.x xmin xmax (Graphics.size_x ()) in
    let posy = calc_pos pt.y ymin ymax (Graphics.size_y ()) in
    (*Printf.printf "%d, %d\n" posx posy; *)
    Graphics.fill_circle posx posy 4;
    Graphics.moveto posx (posy+4);
    Graphics.draw_char (char_of_int (48+pt.i))

let plot_edge = fun (xmin,xmax) (ymin,ymax) pts (i,j) -> 
        let pi = find_point pts i and pj = find_point pts j in
        let posxi = calc_pos pi.x xmin xmax (Graphics.size_x ()) in
        let posyi = calc_pos pi.y ymin ymax (Graphics.size_y ()) in
        let posxj = calc_pos pj.x xmin xmax (Graphics.size_x ()) in
        let posyj = calc_pos pj.y ymin ymax (Graphics.size_y ()) in
        (*Printf.printf "%d, %d -> %d, %d\n" posxi posyi posxj posyj;*)
        Graphics.moveto posxi posyi;
        Graphics.lineto posxj posyj

let plot_graph = fun graph ->
    let xs = List.map (fun p -> p.x) graph.points in
    let ys = List.map (fun p -> p.y) graph.points in
    let dx = iter min xs, iter max xs in
    let dy = iter min ys, iter max ys in
    Graphics.set_color Graphics.blue;
    Graphics.set_line_width 2;
    List.iter (plot_edge dx dy graph.points) graph.edges;
    Graphics.set_color Graphics.black;
    List.iter (plot_point dx dy) graph.points;;

let abs = fun x ->
        if x>=0 then x
        else -x

let clear () = Graphics.clear_graph ()
