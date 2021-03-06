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

(define-library (book-me transform)

  (export input-lines inside-out)

  (import (scheme base)
          (scheme write)
          (book-me gen))

  (include "transform.scm"))
