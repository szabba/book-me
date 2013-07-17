default: book-me.pdf

book-me.pdf: book-me.scm
	chibi-scheme book-me.scm -i book-me.scm | pandoc -o book-me.pdf

README.mkd: book-me.scm
	chibi-scheme book-me.scm -i book-me.scm | pandoc -t markdown -o README.mkd
