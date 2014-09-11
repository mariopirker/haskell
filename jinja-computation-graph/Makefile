
default:
	ghc Main.hs -o compgraph --make

all: parser	default

parser:
	happy Parser/Bcparser.y

dot: 
	ghc GraphViz/Main.hs -o compgraphdot --make

parsing:
	happy Parser/Bcparser.y 
	ghc Parser/Main.hs -o bcparser --make

clean:
	find . -name *.hi | xargs rm
	find . -name *.o | xargs rm

cleanall:
	find . -name *.hi | xargs rm
	find . -name *.o | xargs rm
	find . -name compgraph | xargs rm
	find . -name bcparser | xargs rm

cleantmp:
	rm *~
