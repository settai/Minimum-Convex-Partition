# Makefile

Lib = graphics.cma str.cma

prog : main.cmo
	ocamlc -o prog $(Lib) types.cmo scanner.cmo graph.cmo enveloppe_convexe.cmo delaunay.cmo triangle.cmo main.cmo

triangle.cmo : delaunay.cmo types.cmo enveloppe_convexe.cmo
	ocamlc -c types.cmo delaunay.cmo enveloppe_convexe.cmo triangle.ml

main.cmo : types.cmo graph.cmo scanner.cmo triangle.cmo
	ocamlc -c types.cmo graph.cmo scanner.cmo triangle.cmo main.ml

types.cmo : types.ml
	ocamlc -c types.ml

delaunay.cmo : types.cmo delaunay.ml
	ocamlc -c types.cmo delaunay.ml

enveloppe_convexe.cmo : types.cmo
	ocamlc -c types.cmo enveloppe_convexe.ml

graph.cmo : types.cmo graph.ml
	ocamlc -c types.cmo graph.ml

scanner.cmo : types.cmo scanner.ml
	ocamlc -c str.cma types.cmo scanner.ml

# Clean
clean:
	rm -f prog *.cm[io] *~
