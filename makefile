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
	rm -f ./*.hi ./*.o ./*.out $(EXE).exe $(EXE) core
	rm -rf ./parsec-3.1.3

stu: stutest.out

stutest.out: compiler
	@./$(EXE) $(RUNFLAGS) stutest1.in > stutest1.out
	@./$(EXE) $(RUNFLAGS) stutest2.in > stutest2.out
	@./$(EXE) $(RUNFLAGS) stutest3.in > stutest3.out
	@./$(EXE) $(RUNFLAGS) stutest4.in > stutest4.out
	@./$(EXE) $(RUNFLAGS) stutest5.in > stutest5.out
	@./$(EXE) $(RUNFLAGS) stutest6.in > stutest6.out
	@./$(EXE) $(RUNFLAGS) stutest7.in > stutest7.out
	@./$(EXE) $(RUNFLAGS) stutest8.in > stutest8.out
	@echo ""
	@echo ""
	@cat stutest1.in stutest1.out
	@echo ""
	@cat stutest2.in stutest2.out
	@echo ""
	@cat stutest3.in stutest3.out
	@echo ""
	@cat stutest4.in stutest4.out
	@echo ""
	@cat stutest5.in stutest5.out
	@echo ""
	@cat stutest6.in stutest6.out
	@echo ""
	@cat stutest7.in stutest7.out
	@echo ""
	@cat stutest8.in stutest8.out

proftest.out: compiler
	cat $(PROFTEST)
	$(EXE) $(PROFTEST) > proftest.out
	cat proftest.out
