entangled-domain-py
===================

+ Description:

  The entangled abstract domain is built upon previous pyana project 
  using Z3 Racket binding to prove list  in bound for demonstration.

  There're two implementations routes, both are implemented in the way
  as the paper described: 
  
  + Concolic relational abstract domain (Conlic RAD):
    The main idea is to use concrete values validated by abstract interpretation (AI)
    This implementation has passed all the test cases.
    
  + The entangled abstract domain (entangled RAD):
    This one aims to use uninterpreted array to model list. Propositions are also
    managed and validated by AI. In addition, it uses per-state assumption base.
    Due to the imprecision of the premiere analysis model(CESK* machine), widening
    is applied here to speed up reaching fix-point. However, it still takes moments
    to verify challenging examples. eg. complex.py.anormalnt
    
+ Related files:

  + The concolic RAD:
    * pycesk3-smt-summarize.rkt
    * pycesk3-smt.rkt
    * utils_cesk3_smt.rkt
    * folder sv_tests-conc-res includes the sample output files (z3 files and verified results)
    
  + The entangled RAD:
    src:  in the smt-list-bound folder
    sample output files: smt-list-bound/sv-tests-part-res
    
  + The python test source file:
    sv_test/test_pyfiles
    sv_test/*.* IR files generated from the pipelines of the compiler.

  + Other framework files can be ignored.

  
+ How to run:

  * Environments:
    * Racket 5.2.1
    * `z3.rkt` requires Z3 4.0, which you can download for your platform from [the
      Microsoft Research
      site](http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html).
      We work on Windows, Mac and Linux. You need to copy or create a symlink to `z3.dll`,
      `libz3.so` or `libz3.dylib` in this directory.
      
 * For simplicity, you don't have to generate IR files from the test python files.
   They have been generated for you.

   You can go into pyana folder:
   make pycesk - to compile the concolic RAD
   make verify-conc-tests - to run the test files

   go to smt-list-bound folder:
   make pycesk - compile the entangled RAD
   make verify-part-tests - to run the test files.

 * If you want to test your own python source files:

   go to pyana directory:

   make compile - to compile all including lexer, parser, translator, desugar,
     	  anormalizer and the relational analysis.
   sh ./Make1Py.sh your/python/file 0 4 - to generate the anormalized IR
   ./pycesk3-smt-summarize-rkt the/generated/anormalized-file

   Or

   ./pycesk3-summarize-smt /the/generated/anormalized-file


   
For any question to run, please contact liangsy@cs.utah.edu
=======
entangled-domain-py
===================
>>>>>>> 026799ac8893ff2ffe71affad992ceb79eac62b2
shuying@pegasus:/Volumes/MI/pp/sas2013/entangled-abstract-domain$ git rm LICENSErm 'LICENSE'
shuying@pegasus:/Volumes/MI/pp/sas2013/entangled-abstract-domain$ git commit -m 'clean up'
[master 593baaa] clean up
 Committer: Shuying Liang <shuying@pegasus.cs.utah.edu>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author

 2 files changed, 22 deletions(-)
 delete mode 100644 .DS_Store
 delete mode 100644 LICENSE
shuying@pegasus:/Volumes/MI/pp/sas2013/entangled-abstract-domain$ git push origin master
Counting objects: 3, done.
Delta compression using up to 24 threads.
Compressing objects: 100% (2/2), done.
Writing objects: 100% (2/2), 234 bytes, done.
Total 2 (delta 1), reused 0 (delta 0)
To https://github.com/shuyingliang/entangled-domain-py
   7cc588b..593baaa  master -> master
shuying@pegasus:/Volumes/MI/pp/sas2013/entangled-abstract-domain$ ls
CRAPL-LICENSE.txt      examples               tests
README.md              main.rkt               utils.rkt
builtins.rkt           parser.rkt             z3-wrapper.rkt
decription_z3rkt.md    pyana                  z3.rkt.sublime-project
derived.rkt            run-tests.rkt
shuying@pegasus:/Volumes/MI/pp/sas2013/entangled-abstract-domain$ cat README.md entangled-domain-py
===================<<<<<<< HEAD
+ Description:

  The entangled abstract domain is built upon previous pyana project 
  using Z3 Racket binding to prove list  in bound for demonstration.

  There're two implementations routes, both are implemented in the way
  as the paper described: 
  
  + Concolic relational abstract domain (Conlic RAD):
    The main idea is to use concrete values validated by abstract interpretation (AI)
    This implementation has passed all the test cases.
    
  + The entangled abstract domain (entangled RAD):
    This one aims to use uninterpreted array to model list. Propositions are also
    managed and validated by AI. In addition, it uses per-state assumption base.
    Due to the imprecision of the premiere analysis model(CESK* machine), widening
    is applied here to speed up reaching fix-point. However, it still takes moments
    to verify challenging examples. eg. complex.py.anormalnt
    
+ Related files:

  + The concolic RAD:
    * pycesk3-smt-summarize.rkt
    * pycesk3-smt.rkt
    * utils_cesk3_smt.rkt
    * folder sv_tests-conc-res includes the sample output files (z3 files and verified results)
    
  + The entangled RAD:
    src:  in the smt-list-bound folder
    sample output files: smt-list-bound/sv-tests-part-res
    
  + The python test source file:
    sv_test/test_pyfiles
    sv_test/*.* IR files generated from the pipelines of the compiler.

  + Other framework files can be ignored.

  
+ How to run:

  * Environments:
    * Racket 5.2.1
    * `z3.rkt` requires Z3 4.0, which you can download for your platform from [the
      Microsoft Research
      site](http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html).
      We work on Windows, Mac and Linux. You need to copy or create a symlink to `z3.dll`,
      `libz3.so` or `libz3.dylib` in this directory.
      
 * For simplicity, you don't have to generate IR files from the test python files.
   They have been generated for you.

   You can go into pyana folder:
   make pycesk - to compile the concolic RAD
   make verify-conc-tests - to run the test files

   go to smt-list-bound folder:
   make pycesk - compile the entangled RAD
   make verify-part-tests - to run the test files.

 * If you want to test your own python source files:

   go to pyana directory:

   make compile - to compile all including lexer, parser, translator, desugar,
   		  anormalizer and the relational analysis.
   sh ./Make1Py.sh your/python/file 0 4 - to generate the anormalized IR
   ./pycesk3-smt-summarize-rkt the/generated/anormalized-file

   Or

   ./pycesk3-summarize-smt /the/generated/anormalized-file


   
For any question to run, please contact liangsy@cs.utah.edu
