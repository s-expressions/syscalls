;;; Parse syscalls.master

(import (scheme base) (scheme write))

;;

(define (read-eof-object)
  (let ((char (peek-char)))
    (and (eof-object? char) char)))

(define (read-the-char goal)
  (let ((char (peek-char)))
    (and (not (eof-object? char))
         (char=? goal char)
         (read-char))))

(define (read-matching-char match?)
  (let ((char (peek-char)))
    (and (not (eof-object? char))
         (match? char)
         (read-char))))

(define (read-matching-chars match?)
  (let loop ((chars #f))
    (let ((char (read-matching-char match?)))
      (if (not char) (and chars (list->string (reverse chars)))
          (loop (cons char (or chars '())))))))

;;

(define (char-horizontal? char)
  (not (char=? char #\newline)))

(define (char-punctuation? char)
  (case char ((#\< #\> #\{ #\} #\( #\) #\[ #\] #\. #\, #\; #\* #\- #\|) #t) (else #f)))

(define (char-identifier? char)
  (or (char<=? #\0 char #\9)
      (char<=? #\A char #\Z)
      (char<=? #\a char #\z)
      (case char ((#\_ #\# #\$ #\. #\/) #t) (else #f))))

(define (skip-whitespace-and-comments)
  (cond ((read-matching-char char-whitespace?)
         (skip-whitespace-and-comments))
        ((read-the-char #\;)
         (let loop ()
           (if (read-matching-char char-horizontal?) (loop)
               (skip-whitespace-and-comments))))))

(define (read-token)
  (skip-whitespace-and-comments)
  (or (read-eof-object)
      (read-matching-char char-punctuation?)
      (read-matching-chars char-identifier?)
      (error "Huh?" (read-char))))

(define (read-all-tokens)
  (let loop ((tokens '()))
    (let ((token (read-token)))
      (if (eof-object? token) (reverse tokens) (loop (cons token tokens))))))

(define (writeln x) (write x) (newline))
(for-each writeln (read-all-tokens))
