all:	pyparse pytrans pydesugar pyanormal pyanormal-nt

pyparse:
	@raco exe pyparse.rkt

pytrans:
	@raco exe ../pytrans2.rkt

pydesug:
	@raco exe pydesugar.rkt

pyanormal:
	@raco exe anormalpy.rkt

pyanormal-nt:
	@raco exe ../anormalpy-nt.rkt


gen-parse:
	@for i in ./*.py; do ../pylex2 < $$i | ../pyparse > $$i.parsed; done


gen-trans:
	@for i in ./*.py; do ../pytrans2 < $$i.parsed > $$i.trans; cat ../hir-header.rkt $$i.trans > $$i.trans.rkt; racket  $$i.trans.rkt; echo  $$i.trans.rkt; python3 $$i; echo  $$i; done
	

gen-desug:
	@for i in ./*.py; do ../pydesugar < $$i.trans > $$i.desug; cat ../lir-header.rkt $$i.desug > $$i.desug.rkt;  racket  $$i.desug.rkt; echo  $$i.desug.rkt; python3 $$i; echo  $$i; done
	
gen-anormal:
	@for i in ./*.py; do ../anormalpy < $$i.desug > $$i.anormal; cat ../anormal-header.rkt $$i.anormal > $$i.anormal.rkt;  racket  $$i.anormal.rkt; echo  $$i.anormal.rkt; python3 $$i; echo  $$i; done
	
gen-anormal-nt:
	@for i in ./*.py; do ../anormalpy-nt < $$i.desug > $$i.anormalnt; echo  $$i; done


	





