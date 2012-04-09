#lang racket
(require (for-syntax racket/date))
(begin-for-syntax
  (printf "This program is being compiled at ~a\n"
          (date->string (current-date))))