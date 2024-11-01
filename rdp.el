;;; rdp.el --- A simple recursive descent parser  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2024 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2024
;; Version: 1.0
;; Keywords: text bibtex
;; URL: https://github.com/joostkremers/parsebib
;; Package-Requires: ((emacs "25.1"))

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;

;;; Code:

(require 'parsebib)

;;; Parser primitives

(define-error 'rdp-error "[RDP] Parse error" 'error)

(defun rdp-parse (&optional end)
  "Parse region between point and END.
If END is omitted or nil, it defaults to point-max."
  (or end (setq end (point-max)))
  (let ((res (rdp-start (point) end)))
    ;; After parsing, we should have reached `end':
    (if (= (point) end)
        res
      (signal 'rdp-error (list (format "Expected EOF, got `%c' at position %d." (following-char) (point)))))))

(defun rdp-skip-whitespace ()
  "Skip whitespace."
  (skip-chars-forward " \n\r\t\f\v"))

(defun rdp-comment-line ()
  "Read a line as text and return it."
  (prog1 (buffer-substring-no-properties (point) (pos-eol))
    (forward-line 1)))

(defun rdp-char (chars &optional noerror)
  "Return the character at point if it matches CHARS.
CHARS is a string and should describe a set of characters, as in
a character alternative in `[...]' in a regular expression.  If
the character at point matches CHARS, return it and move point,
otherwise signal an error, unless NOERROR is non-nil, in which
case return nil."
  (rdp-skip-whitespace)
  (if (looking-at-p (concat "[" chars "]"))
      (let ((char (char-after)))
        (forward-char 1)
        char)
    (unless noerror
      (signal 'rdp-error (list (format "Expected [%s], got `%c' at position %d" chars (following-char) (point)))))))

(defun rdp-keyword (keywords &optional noerror)
  "Return the keyword following point.
KEYWORDS is a list of allowed keywords.  If the text following
point matches one of KEYWORDS, return it and move point.
Otherwise signal an error, unless NOERROR is non-nil, in which
case return nil.  This function first skips whitespace before
checking for KEYWORDS."
  (rdp-skip-whitespace)
  (if (looking-at (regexp-opt keywords))
      (let ((keyword (match-string-no-properties 0)))
        (progn
          (goto-char (match-end 0))
          keyword))
    (unless noerror
      (signal 'rdp-error (list (format "Expected one of %s, got `%c' at position %d" keywords (char-after) (point)))))))

(defun rdp-symbol (regexp &optional noerror)
  "Read a symbol and return it.
REGEXP is a regular expression describing a licit symbol.  If a
symbol is found, return it.  Otherwise signal an error, unless
NOERROR is non-nil, in which case return nil."
  (rdp-skip-whitespace)
  (if (looking-at regexp)
      (progn
        (goto-char (match-end 0))
        (match-string-no-properties 0))
    (unless noerror
      (signal 'rdp-error (list (format "Illegal identifier at position %d" (point)))))))

(defun rdp-seq-delim (open close esc)
  "Read a delimited sequence.
A delimited sequence is a sequence delimited by OPEN and CLOSE
characters, which must be different (e.g., any kind of
parentheses).  ESC is an escape character that can be used to
escape OPEN and CLOSE inside the sequence.  OPEN and CLOSE can
appear in the sequence unescaped as long as they are
balanced.  (In other words, the sequence can contain nested
sequences)."
  (rdp-skip-whitespace)
  (let ((beg (point))
        (n-braces 1)
        (skip-chars (format "^%c%c" open close)))
    (rdp-char (char-to-string open))
    (while (and (> n-braces 0)
                (not (eobp)))
      (skip-chars-forward skip-chars)
      (cond
       ((eq (char-after) open)
        (unless (eq (char-before) esc)
          (setq n-braces (1+ n-braces))))
       ((eq (char-after) close)
        (unless (eq (char-before) esc)
          (setq n-braces (1- n-braces)))))
      (ignore-error 'end-of-buffer (forward-char 1)))
    (if (> n-braces 0)
        (signal 'rdp-error (list (format "Opening %c at position %d has no closing %c" open beg close)))
      (buffer-substring-no-properties beg (point)))))

(defun rdp-string (delim esc)
  "Read a string delimited by DELIM.
A string is a delimited sequence where the opening and closing
delimiters are identical, e.g., \"...\".  ESC is the escape
character."
  (rdp-skip-whitespace)
  (let ((beg (point))
        (continue t)
        (skip-chars (format "^%c" delim)))
    (rdp-char (char-to-string delim))
    (while (and continue
                (not (eobp)))
      (skip-chars-forward skip-chars)
      (unless (eq (char-before) esc)
        (setq continue nil))
      (forward-char 1))
    (if continue
        (signal 'rdp-error (list (format "Opening %c at position %d has no closing %c" delim beg delim)))
      (buffer-substring-no-properties beg (point))))  )

(defun rdp-match (rules)
  "Check if any of RULES matches at point.
If a rule matches, apply it and return the result.  Otherwise,
signal an error."
  (rdp-skip-whitespace)
  (let ((start-pos (point))
        last-error)
    (catch 'success
      (dolist (rule rules)
        (condition-case err
            (let ((res (funcall rule)))
              (rdp-skip-whitespace)
              (throw 'success res))
          (rdp-error
           (goto-char start-pos)
           (setq last-error err))))
      (signal (car last-error) (cdr last-error)))))

;;; Parser rules

(defun rdp-start (beg end)
  "Parse region between BEG and END."
  (cons beg end))

;; Basic building blocks

(defun rdp-text ()
  "Parse text.
Text is anything that is between braces or double quotes that
should be read literally, primarily @String expansions and field
values."
  (rdp-match '(rdp-braced-text
               rdp-quoted-text)))

(defun rdp-braced-text ()
  "Parse text in curly braces."
  (rdp-seq-delim ?\{ ?\} ?\\))

(defun rdp-quoted-text ()
  "Parse text in double quotes."
  (rdp-string ?\" ?\\))

(defun rdp-identifier ()
  "Parse a BibTeX identifier."
  (rdp-symbol parsebib--bibtex-identifier))

(defun rdp-composed-value ()
  "Parse a BibTeX composed field value."
  (let ((val (list (rdp-value))))
    (while (and (rdp-char "#" :noerror)
                (not (eobp)))
      (push (rdp-value) val))
    (string-join (nreverse val) " # ")))

(defun rdp-value ()
  "Parse a BibTeX field value."
  (rdp-match '(rdp-text
               rdp-identifier)))

(defun rdp-assignment ()
  "Parse a BibTeX assignment."
  (if-let* ((id (rdp-identifier))
            ((rdp-char "="))
            (val (rdp-composed-value)))
      (cons id val)
    (signal 'rdp-error (list (format "Malformed key=value assignment at position %d" (point))))))

(defun rdp-fields ()
  "Parse a set of BibTeX assignments."
  (let ((fields (list (rdp-assignment))))
    (while (and (rdp-char "," :noerror)
                (not (eobp)))
      (push (rdp-assignment) fields))
    fields))

;; BibTeX items

(defun rdp-@Comment ()
  "Parse a @Comment."
  (rdp-char "@")
  (rdp-keyword '("comment"))
  (or (rdp-match '(rdp-text
                   rdp-comment-line))
      (signal 'rdp-error (list (format "Malformed @Comment at position %d" (point))))))

(defun rdp-@Preamble ()
  "Parse a @Preamble."
  (rdp-char "@")
  (rdp-keyword '("preamble"))
  (or (rdp-text)
      (signal 'rdp-error (list (format "Malformed @Preamble at position %d" (point))))))

(defun rdp-@String ()
  "Parse an @String definition."
  (if-let* (((rdp-char "@"))
            ((rdp-keyword '("string")))
            ((rdp-char "{("))
            (definition (rdp-assignment))
            ((rdp-char "})")))
      definition
    (signal 'rdp-error (list (format "Malformed @String definition at position %d" (point))))))

(defun rdp-entry ()
  "Parse a BibTeX database entry."
  (if-let* (((rdp-char "@"))
            (type (rdp-identifier))
            ((rdp-char "{("))
            (key (rdp-identifier))
            ((rdp-char ","))
            (fields (rdp-fields))
            ((rdp-char "})")))
      (progn (push (cons "=type=" type) fields)
             (push (cons "=key=" key) fields)
             fields)
    (signal 'rdp-error (list (format "Malformed entry definition at position %d" (point))))))

(provide 'rdp)

;;; rdp.el ends here
