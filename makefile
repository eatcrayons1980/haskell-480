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
	$(CCC) $(CCFLAGS) $(SOURCE) -o $(EXE)

clean:
	rm -f ./*.hi ./*.o ./*.out $(EXE) core
	ls

stu: stutest.out

stutest.out: compiler
	-./$(EXE) $(RUNFLAGS) stutest1.in > stutest1.out
	cat stutest1.out
	-./$(EXE) $(RUNFLAGS) stutest2.in > stutest2.out
	cat stutest2.out
	-./$(EXE) $(RUNFLAGS) stutest3.in > stutest3.out
	cat stutest3.out
	cat stutest1.in stutest1.out stutest2.in stutest2.out stutest3.in stutest3.out

proftest.out: compiler
	cat $(PROFTEST)
	$(EXE) $(PROFTEST) > proftest.out
	cat proftest.out
