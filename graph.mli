(** Les outils pour afficher le graph *)

val find_point : Types.point list -> int -> Types.point
(** revoyer le point a partir de sa position  *)

val calc_pos : int -> int -> int -> int -> int
(** calculer la position de la cordonne dans la fenetre *)

val plot_point : int * int -> int * int -> Types.point -> unit
(** dessiner les points dans la fenetre *)

val plot_edge :
  int * int -> int * int -> Types.point list -> int * int -> unit
(** dessiner les trais dans la fenetre *)

val plot_graph : Types.graph -> unit list
(** dessiner le graphe dans la fenetre *)

val abs : int -> int
(** la valeur absolue *)

