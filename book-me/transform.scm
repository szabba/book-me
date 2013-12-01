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

;;; `empty-line` and `code-indent` are just convenient names for certain
;;; constants.

(define empty-line "")
(define code-indent "    ")
