all:	
	ghc --make -O2 TI/Main.hs -o Main

clean:
	rm -rf compiled Main TI/PyData.hi TI/PyData.o TI/PyEval.hi TI/PyEval.o TI/Utils.hi TI/Utils.o TI/HskDataTranslator.hi TI/HskDataTranslator.o TI/Main.hi TI/Main.o
	
test:
	@for i in TI/tests/*.cps2; do ./Main $$i; echo $$i; done
test-one:	
	./Main "/home/shuying/hs/TI/tests/ttt.py.cps2"
													
	
													

