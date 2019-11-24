# Makefile

Lib = graphics.cma str.cma

prog : graph.cmo scanner.cmo
	ocamlc -o prog $(Lib) scanner.cmo graph.cmo

graph.cmo : scanner.cmo
	ocamlc -c scanner.cmo graph.ml 
	
scanner.cmo : scanner.ml
	ocamlc -c str.cma scanner.ml

# Clean 
clean:
	rm -f prog *.cm[io] *~
