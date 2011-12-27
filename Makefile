all: README.md README.ja.md

README.md: README.tmpl 9deg
	python gen_readme.py $< $@

README.ja.md: README.ja.tmpl 9deg
	python gen_readme.py $< $@
