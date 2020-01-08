exception Poly_found of Types.polygone
exception Error

(** La triangulation naive *)

val vecteur : Types.point -> Types.point -> int * int
(** *)

val determinant : int * int -> int * int -> int
(** *)

val saillant : Types.point -> Types.point -> Types.point -> bool
(** *)

val rentrant : Types.point -> Types.point -> Types.point -> bool
(** *)

val alignes : Types.point -> Types.point -> Types.point -> bool
(** *)

val carre : int -> int
(** *)

val d2 : int * int -> int * int -> int
(** *)

val cercle_circonscrit :
  int * int -> int * int -> int * int -> (int * int) * float
(** *)
  
val sens_direct : Types.point -> Types.point -> Types.point -> bool
(** *)

val est_triangle : Types.point -> Types.point -> Types.point -> bool
(** indique si un triplé est un triangle ou non *)

val est_dans_triangle : Types.point list -> Types.point -> bool
(** Prends en entree un triangle et un point et renvoie un booleen pour indiquer si le point est dans le triangle ou non *)


val strict_dans_triangle : Types.point list -> Types.point -> bool
(** Prends en entree un triangle et un point et renvoie un booleen pour indiquer si le point est strictement dans le triangle ou non *)

val obtention_enveloppe : Types.point list -> Types.point list
(** Prends en entrée une liste de points et renvoie les points de l'enveloppe sous forme de l'enveloppe *)

val split :
  Types.point list ->
  Types.point list ->
  'a ->
  Types.point list -> Types.point list * Types.point list * Types.point list
(**  Prends en entrée 3 triangle et une liste de points à l'intérieur de ces triangles et renvoie 3 listes de point à l'intérieur de chaque triangle. *)

val split2 :
  Types.point list list -> Types.point list -> Types.point list list
(** Prends en entrée une liste de triangle [a,b,c,...] et une liste de points à l'intérieur de ces triangles et renvoie une liste de liste de point à l'intérieur de chque triangle. *)

val split_triangle : 'a list -> 'a -> 'a list list
(** Prends en entrée un triangle et un point et renvoie tous les triangles entre ce points et les
points du polygone. *)

val split_liste_in_triangle : 'a list -> 'a -> 'a list list
(**Prends en entrée un polygone et un point et renvoie tous les triangles entre ce points et les
points du polygone. *)

val tri_to_edge : Types.point list list -> (int * int) list
(** Prend en entrée un elt et une liste et renvoie un booleen signifiant que l elt est dans la liste. *)

val print_triangle : Types.point * Types.point * Types.point -> unit
(** Affiche les points composant le triangle sur le terminal. *)

val in_list : 'a -> 'a list -> bool
(**Prend en entrée un elt et une liste et renvoie un booleen signifiant que l elt est dans la liste. *)

val virer_doublon : 'a list -> 'a list -> 'a list
(** Prend en entrée une liste et une autre liste et renvoiela première liste privée des éléments de la deuxième. *)

val deuxieme_partie_triangulation :
  Types.point list -> Types.point list -> Types.point list list
(** 2éme partie de la triangulation naïve . *)

val det_pt_int : Types.point list -> Types.point list -> Types.point list
(** *)

val fin_triangulation :
  Types.point list list ->
  Types.point list -> Types.point list list -> Types.point list list
(** *)

val triangulation : Types.point list -> Types.point list list
(** Prends une liste de points et renvoie la triangulation naïve. *)

val points_to_poly : Types.point list list -> Types.polygone list
(**  convertis Types.point list en Types.polygone list *)

val delaunay_to_edges : Delaunay.Int.S.point list -> Type.edge list
(** Adaptate la triagulation de delaunay implemantée dans la bible pour notre structure (sous forme d'arrête). *)

val is_convex : Types.point list -> int list -> bool
(**  Teste si un polygone est convex, avec la liste de tous les cordonnes des points du polygone. *)

val poly_fusion : int list -> int list -> Types.polygone
(** Fusionne deux polygone ayant un trait en commun. *)

val delaunay_to_polygones : Delaunay.Int.S.point list -> int list list
(** La triangulation de delaunay ayant comme sortie une liste     de polygone qui represente la liste des triangles *)

val exists_in_poly : 'a -> 'a -> 'a list -> bool
(** Teste si le trait (a,b) existe dans un polygone. *)

val remove_poly : 'a -> 'a list -> 'a list
(** Supprime un polygone de la liste des polygones *)

val poly_to_edges : 'a list -> ('a * 'a) list
(** Transforme un polygone en une liste d'arrêtes. *)

val poly_reduction :
  Types.polygone list -> Types.point list -> (int * int) list
(** Parcours la liste des polygones et pour chaque edge commun entre deux polygone il essaie si on peut l'enlever tout en gardant la convexité, la liste des points sert pour retrouver les cordonnés des references*)

val poly_red_alea :
  Types.polygone list -> Types.point list -> (int * int) list
(** Fait pareil que poly_reduction mais mélange aléatoirement les listes d'arrête pour fournir différentes solutions et récupère la meilleure de toutes.*)

