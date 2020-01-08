(** Les outils pour extraire les donnees du fichier json *)

val get_i : Types.point -> int
(** getter de la position i du point *)

val get_x : Types.point -> int
(** getter de la coordonnee x du point *)

val get_y : Types.point -> int
(** getter de la coordonnee y du point *)

val load_file : string -> bytes
(** lire un fichier et renvoyer son contenue *)

val remove : 'a -> 'a list -> 'a list
(** prendre une liste et un element et renvoyer la liste sans l'element *)

val strip_string : string -> string
(** renvoyer la chaine de caractere sans les whitespace *)

val extract_val : string -> string list
(** extraire les valeur i x y depuis la chaine de caractere *)

val to_points : string list -> Types.point list
(** prendre la liste des i x y et renvoyer une liste de point *)

val input_points : string -> Types.point list
(** prendre le nom du fichier json et renvoyer une liste de point *)

