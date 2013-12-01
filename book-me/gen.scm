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

;;; # `(book-me gen)`
;;;
;;; The module implements Python-like generators on top of `call/cc`.

;;; ## `generator`
;;;
;;; At the very essence of things we have `generator`. It's argument `f`
;;; is a procedure accepting a single argument -- another procedure used
;;; for yielding values from the generator. The value the yielding
;;; procedure returns when calles is unspecified.
;;;
;;; `generator`'s result is the generator specified by `f`.
;;;
;;; Generators support two operations only: seeing whether another value
;;; is available and returning the last one.
;;;
;;; > **NOTE:** Performing those generator operations out of order is an
;;; > error.

(define (generator f)

  (let* ((found (list))
         (return #f)
         (continue
          (lambda (yield)
            (f yield)
            (set! found (list))
            (return #f))))

;;; `found` is used to exchange information between the value-finding
;;; and value-returing part of the generator implementation. Basically,
;;; after `(has-next? g)` returns `#t`, `found` has a single element.
;;; After `(next g)` returns, `found` is empty.
;;;
;;; `return` is the continuation of the last call to `has-value?`. It is
;;; used to report whether the generator has another value to provide to
;;; it's user.
;;;
;;; `continue` is the procedure that gets the yielding procedure passed
;;; as an argument. It wraps `f` so that after `f` is done and no more
;;; calls to `yield` will occur it `return`s `#f` through the last
;;; `has-value?` call's continuation.
;;;
;;; The generator itself is a procedure that takes a symbol identifying
;;; the operation to be performed. This is encapsulated in `has-next?`
;;; and `next` (see below).

    (lambda (action)

;;; `next-value` returns the last value found.
;;;
;;; It happens to be defined first because it was easier to implement.
;;; The implementation of `has-value?` was tailored to be compatible
;;; with it -- not the other way round.
;;;
;;; As a side benefit defining it first made impementing `has-value`
;;; easier -- there was a clearly defined idea of what it should do.

      (define (next-value)

        (unless (pair? found)
          (error "u-gen: no value found yet!"))

        (let ((v (car found)))

          (set! found (list))
          v))

;;; `has-value?` returns a boolean indicating whether another value can
;;; be obtained from the generator by a call to `next`. It captures it's
;;; own continuation in an outside location and finally calls `continue`
;;; with the `yield` as an argument.
;;;
;;; Note that while we get a new `yield` on each call to the dispatching
;;; procedure, `continue` keeps reusing the one passed on the first one.
;;; The ones passed afterwards become the results returned by `yield`
;;; within `f`. This is ok, as we leave the result unspecified to the
;;; user.

      (define (has-value?)
        (call/cc
         (lambda (outside)
           (set! return outside)

           (continue yield))))

;;; `yield` is the helper procedure used to report a value from inside
;;; the inside of the generator. It stores it's continuation -- the
;;; point in `f` from which we intend to resume -- and the value given
;;; to it. After that it returns throug the continuation of the last
;;; call to `has-value?` -- reporting that a value is available.

      (define (yield value)
        (call/cc
         (lambda (rest-of-f)
           (set! continue rest-of-f)
           (set! found (list value))
           (return #t))))

;;; The generator branches on the action tag specified and calls the
;;; appropriate inner procedure.

      (case action
        ((next) (next-value))
        ((has-next?) (has-value?))
        (else (error "generator: Only next and has-next? are supported operations."))))))

;;; ## Example `up-to`
;;;
;;;     (define (up-to n)
;;;       (generator
;;;         (lambda (yield)
;;;
;;;           (let loop ((i 0))
;;;             (when (< i n)
;;;               (yield i)
;;;               (loop (+ i 1)))))))
;;;
;;; `up-to` will be a generator yielding integers from 0 to n - 1.

;;; ## Basic generator operations
;;;
;;; `has-next?` tells whether a generator will return another value and
;;; `next` returns the next value from the generator.

(define (has-next? gen)
  (gen 'has-next?))

(define (next gen)
  (gen 'next))

;;; > NOTE: It is an error to call `has-next?` and `next` in an order
;;; > differing from
;;; >
;;; > * `(has-next? g) => #t`
;;; > * `(next g)      => some-value`
;;; > * `(has-next? g) => #t`
;;; > * `(next g)      => some-value`
;;; > * `...`
;;; > * `(has-next? g) => #f`
;;; >
;;; > In the future, a requirement for `has-next?` to be idempotent
;;; > between calls to `next` might be introduced.

;;; ## Higher order generator operations
;;;
;;; Procedures to map, iterate over and filter generators are provided.
;;;
;;; `generator-map` takes a procedure and a generator, and returns
;;; another generator, yielding all the values of `g` transformed using
;;; `f`.

(define (generator-map f g)
  (generator
    (lambda (yield)

      (let loop ()
        (when (has-next? g)

          (yield (f (next g)))
          (loop))))))

;;; `generator-for-each` takes a procedure and a generator, and applies
;;; `f` to all the values yielded by `g`.

(define (generator-for-each f g)

  (let loop ()
    (when (has-next? g)

      (f (next g))
      (loop))))

;;; `generator-filter` takes a predicate `p` and generator `g`, and
;;; returns a generator yielding the elements of `g` for which `p` is
;;; true.

(define (generator-filter p g)
  (generator
    (lambda (yield)

      (let loop ()
        (when (has-next? g)

          (let ((v (next g)))

            (when (p v)
              (yield v)))

          (loop))))))

;;; `generator-append` takes an arbitrary number of generators and
;;; returns a generator that will yield all their values in order.

(define (generator-append . gs)
  (generator
    (lambda (yield)

      (for-each
        (lambda (g)
          (generator-for-each yield g))
        gs))))
