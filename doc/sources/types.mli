(** Module regroupant les types les plus importants utilisés dans la plus part des modules *)

(** Le record point est créer de la même manière que dans le json *)
type point = {
  i : int; (**i est l'indice du point *)
  x : int; (**x est l'absisse du point*)
  y : int; (**y est l'ordonné du point*)
}

(** Couple des identifiants des deux points. Le type edge est créer de la même manière que dans le json *)
type edge = int * int

(** La representation d'un graphe. *)
type graph = {
  points : point list; (** la liste des points du graphe *)
  edges : edge list; (** la liste des arrêtes du graphe *)
}