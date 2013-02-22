#--------------------------------------------
# INSTRUCTION
# Quoted strings are to be filled in by student
#
SOURCE = Parser.hs
EXE = parser
CCC = ghc
CCFLAGS = --make
RUNFLAGS =

default: compiler

compiler:
	chmod 777 check_parsec.sh
	./check_parsec.sh
	$(CCC) $(CCFLAGS) $(SOURCE) -o $(EXE)

clean:
	rm -f ./*.hi ./*.o ./*.out $(EXE) core
	rm -rf ./parsec-3.1.3

stu: stutest.out

stutest.out: compiler
	-./$(EXE) $(RUNFLAGS) stutest1.in > stutest1.out
	-./$(EXE) $(RUNFLAGS) stutest2.in > stutest2.out
	-./$(EXE) $(RUNFLAGS) stutest3.in > stutest3.out
	-./$(EXE) $(RUNFLAGS) stutest4.in > stutest4.out
	-./$(EXE) $(RUNFLAGS) stutest5.in > stutest5.out
	-./$(EXE) $(RUNFLAGS) stutest6.in > stutest6.out
	-./$(EXE) $(RUNFLAGS) stutest7.in > stutest7.out
	cat stutest1.in stutest1.out stutest2.in stutest2.out stutest3.in stutest3.out \
		stutest4.in stutest4.out stutest5.in stutest5.out stutest6.in stutest6.out \
		stutest7.in stutest7.out

proftest.out: compiler
	cat $(PROFTEST)
	$(EXE) $(PROFTEST) > proftest.out
	cat proftest.out
