#--------------------------------------------
# INSTRUCTION
# Quoted strings are to be filled in by student
#
SOURCE = Parser.hs
EXE = parser
CCC = ghc
CCFLAGS = --make
RUNFLAGS =
GFLAGS = -e bye

default: compiler

compiler:
	chmod 777 check_parsec.sh
	./check_parsec.sh
	$(CCC) $(CCFLAGS) $(SOURCE) -o $(EXE)

clean:
	rm -f ./*.fs ./*.hi ./*.o ./*.out $(EXE).exe $(EXE) core
	rm -rf ./parsec-3.1.3

stu: stutest.out

stutest.out: 1 2 3 4 5 6 7 8 9

1: compiler
	@./$(EXE) $(RUNFLAGS) stutest1.in > stutest1.fs
	@gforth stutest1.fs $(GFLAGS) > stutest1.out
	@echo ""
	@echo ""
	cat stutest1.in
	@echo ""
	cat stutest1.fs
	cat stutest1.out
	@echo ""
	@echo ""

2: compiler
	@./$(EXE) $(RUNFLAGS) stutest2.in > stutest2.fs
	@gforth stutest2.fs $(GFLAGS) > stutest2.out
	@echo ""
	@echo ""
	cat stutest2.in
	@echo ""
	cat stutest2.fs
	cat stutest2.out
	@echo ""
	@echo ""

3: compiler
	@./$(EXE) $(RUNFLAGS) stutest3.in > stutest3.fs
	@gforth stutest3.fs $(GFLAGS) > stutest3.out
	@echo ""
	@echo ""
	cat stutest3.in
	@echo ""
	cat stutest3.fs
	cat stutest3.out
	@echo ""
	@echo ""

4: compiler
	@./$(EXE) $(RUNFLAGS) stutest4.in > stutest4.fs
	@gforth stutest4.fs $(GFLAGS) > stutest4.out
	@echo ""
	@echo ""
	cat stutest4.in
	@echo ""
	cat stutest4.fs
	cat stutest4.out
	@echo ""
	@echo ""

5: compiler
	@./$(EXE) $(RUNFLAGS) stutest5.in > stutest5.fs
	@gforth stutest5.fs $(GFLAGS) > stutest5.out
	@echo ""
	@echo ""
	cat stutest5.in
	@echo ""
	cat stutest5.fs
	cat stutest5.out
	@echo ""
	@echo ""

6: compiler
	@./$(EXE) $(RUNFLAGS) stutest6.in > stutest6.fs
	@gforth stutest6.fs $(GFLAGS) > stutest6.out
	@echo ""
	@echo ""
	cat stutest6.in
	@echo ""
	cat stutest6.fs
	cat stutest6.out
	@echo ""
	@echo ""

7: compiler
	@./$(EXE) $(RUNFLAGS) stutest7.in > stutest7.fs
	@gforth stutest7.fs $(GFLAGS) > stutest7.out
	@echo ""
	@echo ""
	cat stutest7.in
	@echo ""
	cat stutest7.fs
	cat stutest7.out
	@echo ""
	@echo ""

8: compiler
	@./$(EXE) $(RUNFLAGS) stutest8.in > stutest8.fs
	@gforth stutest8.fs $(GFLAGS) > stutest8.out
	@echo ""
	@echo ""
	cat stutest8.in
	@echo ""
	cat stutest8.fs
	cat stutest8.out
	@echo ""
	@echo ""

9: compiler
	@./$(EXE) $(RUNFLAGS) stutest9.in > stutest9.fs
	@gforth stutest9.fs $(GFLAGS) > stutest9.out
	@echo ""
	@echo ""
	cat stutest9.in
	@echo ""
	cat stutest9.fs
	cat stutest9.out
	@echo ""
	@echo ""

proftest.out: compiler
	cat $(PROFTEST)
	$(EXE) $(PROFTEST) > proftest.out
	cat proftest.out
