#lang racket
(require (for-syntax racket/date
                     (planet dyoo/stardate)))
(begin-for-syntax
  (printf "This program is being compiled at Stardate ~a\n"
          (date->stardate (current-date))))