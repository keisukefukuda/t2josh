#all: README.md README.ja.md

GHC := /home/usr1/11M37264/local/haskell/bin/ghc
LIB_SRC := $(shell find . -name "*.hs")

all: queue_usage run_tests t2josh

README.md: README.tmpl t2josh
	python gen_readme.py $< $@

README.ja.md: README.ja.tmpl t2josh
	python gen_readme.py $< $@

queue_usage: QueueUsage.hs ${LIB_SRC}
	${GHC} --make $< -o $@

run_tests: tests.hs ${LIB_SRC}
	${GHC} --make tests.hs -o $@

test: run_tests
	./$<

t2josh: Main.hs ${LIB_SRC}
	${GHC} --make $< -o $@

clean: 	
	rm -f queue_usage run_tests t2josh
