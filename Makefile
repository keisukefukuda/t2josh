all: README.md README.ja.md

README.md: README.tmpl
	python gen_readme.py $< $@

README.ja.md: README.ja.tmpl
	python gen_readme.py $< $@