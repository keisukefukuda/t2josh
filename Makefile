all: README.md README.ja.md

README.md: README.tmpl t2josh
	python gen_readme.py $< $@

README.ja.md: README.ja.tmpl t2josh
	python gen_readme.py $< $@
