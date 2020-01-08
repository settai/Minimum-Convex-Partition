exception Error

(** La triangulation deterministe *)

val cdt : Types.edge -> Types.polygone list -> Types.point list -> bool
(** cdt est une fonction qui renvoie un booléen indiquant si une arrête est enlevable.*)


val fusion_base_arrete :
  Types.edges -> Types.polygone list -> Types.point list -> Types.polygone list
(** A partir d'une arrête enlevable, fusionne les deux polygones qui seront mpactés par le retrait de l'arrête.  *)


val compte_arrete_enlevable :
  Types.edge list -> int list list -> Types.point list -> int
(**  A partir d'un situation, indique le nombre d'arrêtes maximum que l'on peut enlever. *)


val arrete_localement_fav :
  Types.polygone list -> Types.edge list -> Types.point list -> int * int
(**  cette fonction permet de déterminer qu'elle arrête il vaut mieux enlever si on ne veut pas trop réduire le nombre maximum d'arrêtes que l'on pourrait enlever au maximum. *)



val optim_locale :
  Types.polygone list ->
  (int * int) list -> Types.point list -> Types.polygone list
  (**  cette fonction permet de déterminer qu'elle arrête il vaut mieux enlever si on ne veut pas trop réduire le nombre maximum d'arrêtes que l'on pourrait enlever au maximum. *)
