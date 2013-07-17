;;; % book-me.scm
;;; % by [Karol Marcjan](http://github.org/szabba/) (<karol.marcjan@gmail.com>)
;;; % [Fork me on github!](http://github.org/szabba/book-me)
;;;
;;; `book-me.scm` is an R7RS Scheme script that formats source code for
;;; printing. It strips a so-called comment marker from lines that start
;;; with it, indents code by 4-spaces and does some mumbo-jumbo with
;;; empty lines.
;;;
;;; The overall effect is, that if you format stuff in the lines
;;; prefixed with the comment marker as Markdown it will produce a
;;; Markdown document with the text interwoven with the code.
;;;
;;; In the spirit of `pbook.el`, created by [Luke
;;; Gorrie](http://github.org/lukego), it can be run on itself. Neat,
;;; huh?
;;;
;;; # License
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; # Imports
;;;
;;; R7RS introduced a new library system. It is by design incompatible
;;; with the one present in R6RS.

(import (scheme base)
        (scheme case-lambda)
        (scheme write)
        (scheme file)
        (scheme process-context))

;;; * `(scheme base)` is a library that corresponds to what one would
;;; call built-ins in other languages.
;;; * `(scheme write)` contains the *O* of the I/O.
;;; * `(scheme file)` gives us the `with-input-from-file` and
;;; `with-output-to-file` procedures. Those can come in handy.
;;; * `(scheme process-context)`, amongst other things, gives us access
;;; to the command line arguments through the `command-line` procedure.
;;;
;;; # Helpers and utilities
;;;
;;; A more descriptive name for the empty string. The intent is
;;; expressed better when using it.

(define empty-line "")

;;; As above, only we name the indent that precedes code in Markdown.

(define code-indent "    ")

;;; Strips the prefix (and a following space) from a text line or
;;; returns false.

(define (strip-text-prefix line prefix)

  (let ((prefix-length (string-length prefix))
        (line-length (string-length line)))

;;; For lines that do not start with the prefix, return false.

    (if (< line-length prefix-length)
      #f
      (let ((beginning (substring line 0 prefix-length))
            (rest (substring line prefix-length line-length))
            (rest-length (- line-length
                            prefix-length)))

        (cond ((not (string=? prefix beginning))
               #f)

;;; When the string is just the prefix, return an empty string.

              ((= (- line-length prefix-length) 0)
               empty-line)

;;; When the rest of the string starts with a space, return the part
;;; that follows it.

              ((char=? (string-ref rest 0) #\space)

                 (substring rest 1 rest-length))

;;; Otherwise consider the line to be code after all...

              (else #f))))))

;;; Why that final clause? Well, you see: putting a space after the
;;; comment mark and before text keeps the original code readable.
;;;
;;;Unlike this. Can you imagine reading several lines, not to mention a
;;;whole file, commented like this? Horror.
;;;
;;; Some people's editors will strip whitespace at the end of lines when
;;; saving. That's why a line consisting just of the comment mark is
;;; treated as a text line.
;;;
;;; # Turning stuff inside-out
;;;
;;; This is where the fun happens!
;;;
;;; `inside-out` loops over the lines in the current input port and
;;; writes the transformed document to the current output port.
;;;
;;; The purpose and nature of the transformation are sketched in the
;;; introductory section.

(define (inside-out comment-mark)

  (let loop ((line (read-line))
             (last-was-text #f))

;;; Before considering other options, ensure that we really got a line!

    (cond ((and (not (string? line))
                (not (eof-object? line)))

           (error "Not a proper line!" line))

;;; Exit on EOF, do not call loop.
;;;
;;; Since we have to do *something*, we write out the empty line.

          ((eof-object? line)

           (write-string empty-line))

;;; A literal empty line is outputted as-is and "inherits" the text-line
;;; status from it's predecessor.

          ((string=? line empty-line)

           (unless last-was-text
             (write-string code-indent))

           (write-string empty-line)
           (newline)

           (loop (read-line) last-was-text))

;;; A text line is handled by a helper procedure.

          ((strip-text-prefix line comment-mark)
           =>
           (lambda (stripped)

             (write-string stripped)
             (newline)

             (loop (read-line) #t)))

;;; A non-empty non-text line must be code. Code in Markdown is indented.

          (else

           (write-string code-indent)
           (write-string line)
           (newline)

           (loop (read-line) #f)))))

;;; # Program body
;;;
;;; Temporarily hard-coded call to inside-out. The "`;;;`" comment mark
;;; is intended for Scheme and other LISP dialects.

(inside-out ";;;")

;;; # Generating PDFs
;;;
;;; `pandoc` is pretty good at producing PDFs out of Markdown. It adds
;;; some extensions of it's own to the vanilla format. Try it -- you'll
;;; have a printable version of your code!
