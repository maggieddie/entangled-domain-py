compile:	pylex pyparse pytrans pydesug pyanormal-nt pycesk

pylex:
	@raco exe pylex2.rkt
pyparse:
	@raco exe pyparse.rkt

pytrans:
	@raco exe pytrans2.rkt

pydesug:
	@raco exe pydesugar.rkt

pyanormal:
	@raco exe anormalpy.rkt
pyanormal-nt:
	@raco exe anormalpy-nt.rkt
pycesk:
	@raco exe pycesk3-smt-summarize-rkt.rkt

clean:
	@rm pylex2 pyparse pytrans2 pydesugar anormalpy-nt pycesk

	
fact:	gen-parse gen-trans gen-desug gen-anormal-nt analyze


gen-parse:
	@for i in ./tests2/*.py; do ./pylex2 < $$i | ./pyparse > $$i.parsed; done


gen-trans:
	@for i in ./tests2/*.py; do ./pytrans2 < $$i.parsed > $$i.trans; cat hir-header.rkt $$i.trans > $$i.trans.rkt; racket $$i.trans.rkt; echo $$i.trans.rkt; python3 $$i; echo $$i; done
	

gen-desug:
	@for i in ./tests2/*.py; do ./pydesugar < $$i.trans > $$i.desug; cat lir-header.rkt $$i.desug > $$i.desug.rkt;  racket $$i.desug.rkt; echo $$i.desug.rkt; python3 $$i; echo  $$i; done
	
gen-anormal:
	@for i in ./tests2/*.py; do ./anormalpy < $$i.desug > $$i.anormal; cat anormal-header.rkt $$i.anormal > $$i.anormal.rkt;  racket $$i.anormal.rkt; echo $$i.anormal.rkt; python3 $$i; echo  $$i; done
gen-anormal-nt:
	@for i in ./tests2/*.py; do ./anormalpy-nt < $$i.desug > $$i.anormalnt; echo $$i; done

analyze:
	@for i in ./tests2/*.py; do ./pycesk-summarize < $$i.anormalnt > $$i.typeinfo.txt; echo $$i; done

verify-conc-tests:
	@./pycesk3-smt-summarize-rkt ./sv_test/simple.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/high-order-exam.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/simple-loop.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/high-vertex_perm.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/vertex-perm.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/vertex-perm2.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/vertex-perm_out_of_bound.py.anormalnt
	@./pycesk3-smt-summarize-rkt ./sv_test/complex.py.anormalnt





