(** Module permettant la recherche de la meilleur solution via la construction d'un arbre de possibilité. La construction de la solution se fait par ajout d'arrête. *)

(** Représentation de l'arbre des possibilités. *)
type arbre = 
  | Vide (** l'arbre vide *)
  | Node of Types.edge * arbre list (** le noeud de l'arbre est une arrête, les sous-arbres sont représentés par une liste d'arbre *)
  | End (** arbre vide qui délimite une solution *)

(** Ce type est un point amélioré qui possède les même information que le point, l'appartenance à l'enveloppe convexe en plus. *)
type point_graph = {
  x : int; (** x est l'absisse du point *)
  y : int; (**y est l'ordonné du point*)
  indice : int; (**indice est l'indice du point *)
  env : bool; (** env est un booléen pour savoir si le point appartient à l'enveloppe convexe *)
}


val is_in_env_conv : 'a -> 'a list -> bool
val create_all_edges : point_graph list -> (int * int) array
val conditions : 'a -> 'b -> 'c -> int
val pt_to_ptgraph : Types.point list -> Types.point -> point_graph
val gen_arbre : Types.graph -> Types.point list -> arbre list
val get_sol : arbre list -> Types.edge list option
