# Makefile

Lib = graphics.cma str.cma

prog : main.cmo
	ocamlc -o prog $(Lib) types.cmo scanner.cmo graph.cmo enveloppe.cmo test_graph.cmo make_tree.ml main.cmo

enveloppe.cmo : types.cmo
	ocamlc -c types.cmo enveloppe.ml

main.cmo : types.cmo graph.cmo scanner.cmo enveloppe.cmo make_tree.cmo
	ocamlc -c types.cmo graph.cmo scanner.cmo enveloppe.cmo test_graph.cmo make_tree.cmo main.ml

types.cmo : types.ml
	ocamlc -c types.ml

graph.cmo : types.cmo graph.ml
	ocamlc -c types.cmo graph.ml

scanner.cmo : types.cmo scanner.ml
	ocamlc -c str.cma types.cmo scanner.ml

make_tree.cmo : types.cmo enveloppe.cmo test_graph.cmo make_tree.ml
	ocamlc -c types.cmo test_graph.cmo make_tree.ml

test_graph.cmo : types.cmo test_graph.ml
	ocamlc -c types.cmo test_graph.ml

# Clean
clean:
	rm -f prog *.cm[io] *~

#test
batterie_test.cmo: batterie_test.ml types.cmo make_tree.cmo graph.cmo
	ocamlc -c batterie_test.ml

tests: batterie_test.cmo
	ocamlc -o tests $(Lib) types.cmo graph.cmo enveloppe.cmo test_graph.cmo make_tree.ml batterie_test.cmo