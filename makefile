#--------------------------------------------
# INSTRUCTION
# Quoted strings are to be filled in by student
#
SOURCE = parse.hs
EXE = parse
CCC = ghc
CCFLAGS = --make
RUNFLAGS =

default: compiler

compiler:
	$(CCC) $(CCFLAGS) $(SOURCE) -o $(EXE)

clean:
	rm -f ./*.hi ./*.o ./*.out $(EXE) core
	ls

stutest.out: compiler
	cat stutest1.in
	-$(EXE) $(RUNFLAGS) stutest1.in > stutest1.out
	cat stutest1.out
	cat stutest2.in
	-$(EXE) $(RUNFLAGS) stutest2.in > stutest2.out
	cat stutest2.out

proftest.out: compiler
	cat $(PROFTEST)
	$(EXE) $(PROFTEST) > proftest.out
	cat proftest.out
