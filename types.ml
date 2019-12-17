type point = {
    i : int;
    x : int;
    y : int
}

type edge = int*int

type graph = {
        points : point list;
        edges  : edge list
}

type triangle = point * point * point

type polygone = int list
