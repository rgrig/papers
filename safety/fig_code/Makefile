JAVA=$(wildcard *.java)
TEX=$(patsubst %.java,%.tex,$(JAVA))
CLS=$(patsubst %.java,%.class,$(JAVA))

all: $(TEX) $(CLS)

$(TEX): %.tex: %.java
	pygmentize -o $@ $<

$(CLS): %.class: %.java
	javac $<

clean:
	rm -f $(TEX)
	rm -f *.class
