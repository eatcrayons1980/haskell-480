#--------------------------------------------
# INSTRUCTION
# Quoted strings are to be filled in by student
#
SOURCE = Parser.hs
EXE = parser
CCC = ghc
CCFLAGS = --make

default: compiler

compiler:
	$(CCC) $(CCFLAGS) $(SOURCE) -o $(EXE)

clean:
	rm -f ./*.hi ./*.o ./*.out $(EXE) core
	ls

stu: stutest.out

stutest.out: compiler
	-./$(EXE) stutest1.in > stutest1.out
	-./$(EXE) stutest2.in > stutest2.out
	-./$(EXE) stutest3.in > stutest3.out
	cat stutest1.in stutest1.out stutest2.in stutest2.out stutest3.in stutest3.out

proftest.out: compiler
	cat proftest.in
	- $(EXE) proftest.in > proftest.out
	cat proftest.out
