TEX=$(patsubst %.java,%.tex,$(wildcard *.java))

all: $(TEX)
	javac *.java

$(TEX): %.tex: %.java
	pygmentize -o $@ $<

clean:
	rm -f $(TEX)
	rm -f *.class
