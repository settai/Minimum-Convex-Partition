# Makefile

Lib = graphics.cma str.cma

prog : main.cmo
	ocamlc -o prog $(Lib) types.cmo scanner.cmo graph.cmo enveloppe.cmo main.cmo

enveloppe.cmo : types.cmo
	ocamlc -c types.cmo enveloppe.ml

main.cmo : types.cmo graph.cmo scanner.cmo enveloppe.cmo
	ocamlc -c types.cmo graph.cmo scanner.cmo enveloppe.cmo main.ml

types.cmo : types.ml
	ocamlc -c types.ml

graph.cmo : types.cmo graph.ml
	ocamlc -c types.cmo graph.ml

scanner.cmo : types.cmo scanner.ml
	ocamlc -c str.cma types.cmo scanner.ml

# Clean
clean:
	rm -f prog *.cm[io] *~
