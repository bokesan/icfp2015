.PHONY: all clean

jar = dist/icfp2015.jar

all: $(jar)

dist/icfp2015.jar: $(shell echo src/*/*.java)
	ant jar

clean:
	ant clean

srcdist:
	mkdir -p dist
	tar czf dist/srcdist.tar.gz README README.md Makefile build.xml play_icfp2015 src web haskell lib
