default: book-me.pdf README.mkd docs

clean:
	rm -rf docs
	rm *.pdf

docs: docs/book-me-transform.pdf docs/book-me-gen.pdf book-me.pdf
	mkdir -p docs/
	cp book-me.pdf docs/book-me.pdf

docs/book-me-transform.pdf: book-me/transform.scm book-me.scm
	mkdir -p docs/
	chibi-scheme book-me.scm -i book-me/transform.scm | pandoc -o docs/book-me-transform.pdf

docs/book-me-gen.pdf: book-me/gen.scm book-me.scm
	mkdir -p docs/
	chibi-scheme book-me.scm -i book-me/gen.scm | pandoc -o docs/book-me-gen.pdf

book-me.pdf: book-me.scm
	chibi-scheme book-me.scm -i book-me.scm | pandoc -o book-me.pdf

README.mkd: book-me.scm
	chibi-scheme book-me.scm -i book-me.scm | pandoc -t markdown -o README.mkd
