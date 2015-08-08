.PHONY: all clean

jar = dist/icfp2015.jar

all: $(jar)

dist/icfp2015.jar: $(shell echo src/*/*.java)
	ant jar

clean:
	ant clean
	$(RM) srcdist.tar.gz

srcdist:
	tar czf srcdist.tar.gz README Makefile play_icfp2015 src
