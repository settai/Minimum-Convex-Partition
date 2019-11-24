# Minimum Convex Partition Problem

## Description

The specific problem chosen for the 2020 Challenge is the following:
Given a set S of n points in the plane. The objective is to compute a plane graph with vertex set S (with each point in S having positive degree) that partitions the convex hull of S into the smallest possible number of convex faces. (Note that collinear points are allowed on face boundaries, so all internal angles of a face are at most π.)

#### Instance Example
![Image](https://cgshop.ibr.cs.tu-bs.de/static/competitions/cgshop_2020/instance-example.svg)
Instance with 10 points

#### Solution Example
![Image](https://cgshop.ibr.cs.tu-bs.de/static/competitions/cgshop_2020/solution-example.svg)
Possible solution with 8 faces

The complexity of this problem is still unknown, but approximation algorithms have been proposed; e.g., see Christian Knauer and Andreas Spillner: Approximation Algorithms for the Minimum Convex Partition Problem, SWAT 2006, pp. 232-241.

### Source

https://cgshop.ibr.cs.tu-bs.de/static/competitions/cgshop_2020/solution-example.svg


## Compiling

```
~/Minimum-Convex-Partition$ make
```

