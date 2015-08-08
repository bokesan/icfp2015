.PHONY: all clean

jar = dist/icfp2015.jar

all: $(jar)

dist/icfp2015.jar: $(shell echo src/*/*.java)
	ant jar

clean:
	ant clean

srcdist:
	mkdir -f dist
	tar czf dist/srcdist.tar.gz README Makefile play_icfp2015 src
