COMPILE = ocamlc
COMPILEOPT = ocamlopt
CSLDEP=ocamldep
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDOC= ocamldoc


NORM_FILES= pqueue.ml memory.ml a_star.ml
NORM_OBJS =  $(NORM_FILES:.ml=.cmo)
OPT_OBJS=  $(NORM_FILES:.ml=.cmx)

all : a_star.cma a_star.cmxa doc

doc:
	mkdir doc;$(CAMLDOC) -html -t "A_star" -d doc -intro Intro -hide Pervasives *.mli

a_star.cma: $(NORM_OBJS)
	$(COMPILE) -a -o $@ $(NORM_OBJS)

a_star.cmxa: $(OPT_OBJS)
	$(COMPILEOPT) -a -o $@ $(OPT_OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.mli:
	$(CAMLLEX) $<
.mll.ml:
	$(CAMLLEX) $<
.mly.mli:
	$(CAMLYACC) $<
.mly.ml:
	$(CAMLYACC) $<
.ml.cmo :
	$(COMPILE) -c $<
.mli.cmi :
	$(COMPILE) -c $<
.ml.cmx :
	$(COMPILEOPT) -c $<

cleandoc:
	\rm -Rf doc

clean:
#	\rm -f *.cmo *.cmi *.cmx *.o *.a *.cma *.cmxa .depend *~ parser.ml parser.mli lexer.ml lexer.mli
	\rm -f a_star.cmo a_star.cmi a_star.cmx a_star.o *.a *.cma *.cmxa .depend *~ parser.ml parser.mli lexer.ml lexer.mli

.depend:
	$(CSLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend
