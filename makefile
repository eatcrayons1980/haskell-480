#--------------------------------------------
# INSTRUCTION
# Quoted strings are to be filled in by student
#
CCC = ghc
CCFLAGS =
OBJS =
SOURCE = parse.hs
RUNFLAGS =

stutest.out:
	cat $(SOURCE)
	$(CCC) $(RUNFLAGS) $(SOURCE) 

clean:
	rm parse.o parse.hi parse
	ls

# Notice the next line. The `-' says to ignore the return code. This
# is a way to have multiple tests of errors that cause non-zero return
# codes.

#proftest.out: compiler
#	cat $(PROFTEST)
#	compiler $(PROFTEST) > proftest.out
#	cat proftest.out