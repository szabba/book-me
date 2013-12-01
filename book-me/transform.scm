;;; This file is part of book-me.

;;; book-me is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; book-me is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with book-me.  If not, see <http://www.gnu.org/licenses/>.

;;; # `(book-me transform)`
;;;
;;; The module implements generators and generator building procedures
;;; that are at the core of book-me's functionality.

;;; ## Input generator
;;;
;;; Returns a generator that yields all the lines from the current
;;; input port. Note that it will still use the same input port when
;;; called from a different dynamic environment later on, ie it captures
;;; the `(current-input-port)` and never looks back at the parameter
;;; value.

(define (input-lines)
  (let ((input-port (current-input-port)))
    (generator
      (lambda (yield)

        (let loop ((line (read-line input-port)))
          (unless (eof-object? line)

            (yield line)
            (loop (read-line))))))))

;;; ## The core transform

;;; `empty-line` and `code-indent` are just convenient names for certain
;;; constants.

(define empty-line "")
(define code-indent "    ")

;;; We need a function to tell whether a string starts with the given
;;; prefix.

(define (string-prefix=? str prefix)
  (let ((str-len (string-length str))
        (prefix-len (string-length prefix)))

    (if (< str-len prefix-len)
         #f
         (string=? prefix (string-copy str 0 prefix-len)))))

;;; ## Line handlers
;;;
;;; Line handlers are procedures we use to transform an input line to an
;;; output line. They take a symbol identifying the previous line's type
;;; and the current one and return the transformed line (or false if it
;;; doesn't fit the line type handled by the handler) and the handler
;;; type tag.
;;;
;;; The procedures defined below (namely `text-line` and `code-line`)
;;; return line handlers.  In them we use `or` as a rough `cond`
;;; equivalent. It defaults to returning `#f` when all the other cases
;;; fail, so we don't have to write a `(else #f)` clause.
;;;
;;; Text lines are either equal to a marker, prefixed by it and at least
;;; one space or empty lines following another text line. The "prefixed
;;; with a marker and at least one space" is there so that

;;;This monstrosity isn't treated as text. We don't want people writing
;;;like this. It's horrible!

(define (text-line marker)

  (let ((prefix (string-append marker " ")))
    (lambda (last-t line)

      (values (or (and (string=? line empty-line)
                       (symbol=? 'text-line last-t)
                       empty-line)

                  (and (string-prefix=? line prefix)
                       (string-copy line (string-length prefix)))

                  (and (string=? line marker)
                       empty-line))

              'text-line))))

;;; Code lines are either empty lines following a code line or lines
;;; that are not text lines.

(define (code-line)
  (lambda (last-t line)

    (values (or (and (string=? line empty-line)
                     (symbol=? 'code-line last-t)
                     code-indent)

                (string-append code-indent line))

            'code-line)))

;;; ## Core procedure
;;;
;;; `inside-out` is really just a piece of plumbing that tries different
;;; line handlers in order, on each line.
;;;
;;; Given a marker it returns a procedure that takes a line generator
;;; and returns another one.

(define (inside-out marker)
  (lambda (lines)
    (let ((last-was 'text-line))

      (generator-map
       (lambda (line)

         (let loop ((ts (list (text-line marker)
                              (code-line))))

           (when (null? ts)
             (error "inside-out: Line matches no type." line))

           (let-values (((new-line this-is)
                         ((car ts) last-was line)))

             (if new-line
                 (begin
                  (set! last-was this-is)
                  new-line)

                 (loop (cdr ts))))))
       lines))))
