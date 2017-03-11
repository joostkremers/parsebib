;;; parsebib.el --- A library for parsing bib files  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2017 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2014
;; Version: 2.0
;; Keywords: text bibtex
;; Package-Requires: ((emacs "24.3"))

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

(require 'bibtex)
(require 'cl-lib)
(require 'subr-x) ; for `string-join'.

(defconst parsebib--bibtex-identifier "[^^\"@\\&$#%',={}() \t\n\f]+" "Regexp describing a licit BibTeX identifier.")
(defconst parsebib--key-regexp "[^^\"@\\&$#%',={} \t\n\f]+" "Regexp describing a licit key.")
(defconst parsebib--entry-start "^[ \t]*@" "Regexp describing the start of an entry.")

;; Emacs 24.3 compatibility code.
(unless (fboundp 'define-error)
  ;; This definition is simply copied from the Emacs 24.4 sources
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar (lambda (parent)
                                (cons parent
                                      (or (get parent 'error-conditions)
                                          (error "Unknown signal `%s'" parent))))
                              parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(define-error 'parsebib-entry-type-error "Illegal entry type" 'error)

;;;;;;;;;;;;;;;;;;;;
;; matching stuff ;;
;;;;;;;;;;;;;;;;;;;;

(defun parsebib--looking-at-goto-end (str &optional match)
  "Like `looking-at' but move point to the end of the matching string STR.
MATCH acts just like the argument to MATCH-END, and defaults to
0. Comparison is done case-insensitively."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at str)
        (goto-char (match-end match)))))

(defun parsebib--match-paren-forward ()
  "Move forward to the closing paren matching the opening paren at point.
This function handles parentheses () and braces {}.  Return t if a
matching parenthesis was found.  Note that this function puts
point right before the closing delimiter (unlike e.g.,
`forward-sexp', which puts it right after.)"
  (let ((result (cond
                 ((eq (char-after) ?\{)
                  (parsebib--match-brace-forward))
                 ((eq (char-after) ?\()
                  ;; This is really a hack. We want to allow unbalanced parentheses in
                  ;; field values (BibTeX does), so we cannot use forward-sexp
                  ;; here. For the same reason, looking for the matching paren by hand
                  ;; is pretty complicated. However, balanced parentheses can only be
                  ;; used to enclose entire entries (or @STRINGs or @PREAMBLEs) so we
                  ;; can be pretty sure we'll find it right before the next @ at the
                  ;; start of a line, or right before the end of the file.
                  (let ((beg (point)))
                    (re-search-forward parsebib--entry-start nil 0)
                    (skip-chars-backward "@ \n\t\f")
                    (if (eq (char-after) ?\))
                        ;; if we've found a closing paren, return t
                        t
                      ;; otherwise put the cursor back and signal an error
                      (goto-char beg)
                      (signal 'scan-error (list "Unbalanced parentheses" beg (point-max)))))))))
    (when result
      ;; move point one char back to place it where the rest of parsebib expects it
      (forward-char -1)
      ;; make sure we return t
      result)))

(defun parsebib--match-delim-forward ()
  "Move forward to the closing delimiter matching the delimiter at point.
This function handles braces {} and double quotes \"\". Return t
if a matching delimiter was found."
  (let ((result (cond
                 ((eq (char-after) ?\{)
                  (parsebib--match-brace-forward))
                 ((eq (char-after) ?\")
                  (parsebib--match-quote-forward)))))
    result))

(defun parsebib--match-brace-forward ()
  "Move forward to the closing brace matching the opening brace at point."
  (with-syntax-table bibtex-braced-string-syntax-table
    (forward-sexp 1)
    ;; if forward-sexp does not result in an error, we want to return t
    t))

(defun parsebib--match-quote-forward ()
  "Move to the closing double quote matching the quote at point."
  (with-syntax-table bibtex-quoted-string-syntax-table
    (forward-sexp 1)
    ;; if forward-sexp does not result in an error, we want to return t
    t))

(defun parsebib--parse-value (limit &optional strings)
  "Parse value at point.
Do not parse beyond LIMIT.  Replace @string abbrevs with STRINGS
if non-nil."
  (let (res)
    (while (and (< (point) limit)
                (not (looking-at-p ",")))
      (cond
       ((looking-at-p "[{\"]")
        (let ((beg (point)))
          (parsebib--match-delim-forward)
          (push (buffer-substring-no-properties beg (point)) res)))
       ((looking-at parsebib--bibtex-identifier)
        (push (buffer-substring-no-properties (point) (match-end 0)) res)
        (goto-char (match-end 0)))
       ((looking-at "[[:space:]]*#[[:space:]]*")
        (goto-char (match-end 0)))
       (t (forward-char 1)))) ; so as not to get stuck in an infinite loop.
    (if strings
        (string-join (parsebib--expand-strings (nreverse res) strings))
      (string-join (nreverse res) " # "))))

(defun parsebib--expand-strings (strings abbrevs)
  "Replace STRINGS with expansions in ABBREVS."
  (mapcar (lambda (str)
            (or (gethash str abbrevs)
                (progn (setq str (replace-regexp-in-string "[ \t\n\f]+" " " str))
                       (cond
                        ((string-match "\\`[\"{]\\(.*?\\)[\"}]\\'" str)
                         (match-string 1 str))
                        ((string-match "[0-9]+" str)
                         str)
                        (t str)))))
          strings))

;;;;;;;;;;;;;;;;;;;
;; low-level API ;;
;;;;;;;;;;;;;;;;;;;

(defun parsebib-find-next-item (&optional pos)
  "Find the first (potential) BibTeX item following POS.

This function simply searches for an @ at the start of a line,
possibly preceded by spaces or tabs, followed by a string of
characters as defined by `parsebib--bibtex-identifier'.  When
successful, point is placed right after the item's type, i.e.,
generally on the opening brace or parenthesis following the entry
type, \"@Comment\", \"@Preamble\" or \"@String\".

The return value is the name of the item as a string, either
\"Comment\", \"Preamble\" or \"String\", or the entry
type (without the @). If an item name is found that includes an
illegal character, an error of type `parsebib-entry-type-error'
is raised. If no item is found, nil is returned and point is left
at the end of the buffer.

POS can be a number or a marker and defaults to point."
  (when pos (goto-char pos))
  (when (re-search-forward parsebib--entry-start nil 0)
    (if (parsebib--looking-at-goto-end (concat "\\(" parsebib--bibtex-identifier "\\)" "[[:space:]]*[\(\{]") 1)
        (match-string-no-properties 1)
      (signal 'parsebib-entry-type-error (list (point))))))

(defun parsebib-read-comment (&optional pos)
  "Read the @Comment beginning at the line POS is on.
Return value is the text of the @Comment or nil if no comment is
found.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @Comment entry must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "comment[[:space:]]*[\(\{]"))
    (let ((beg (point))) ; we are right after the opening brace / parenthesis
      (forward-char -1)  ; move back to the brace / paren
      (when (parsebib--match-paren-forward)
        (buffer-substring-no-properties beg (point))))))

(defun parsebib-read-string (&optional pos strings)
  "Read the @String definition beginning at the line POS is on.
If a proper abbreviation and expansion are found, they are
returned as a cons cell (<abbrev> . <expansion>).  Otherwise, nil
is returned.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @String entry must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point.

If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the string's
expansion."
  (interactive)
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "\\(string[[:space:]]*\\)[\(\{]") 1)
    (let ((limit (save-excursion        ; find the position of the matching end parenthesis
                   (parsebib--match-paren-forward)
                   (point))))
      (parsebib--looking-at-goto-end (concat "[({]\\(" parsebib--bibtex-identifier "\\)[[:space:]]*=[[:space:]]*"))
      (let ((abbr (match-string-no-properties 1)))
        (when (and abbr (> (length abbr) 0))            ; if we found an abbrev
          (let ((expansion (parsebib--parse-value limit strings)))
            (goto-char (1+ limit))
            (cons abbr expansion)))))))

(defun parsebib-read-preamble (&optional pos)
  "Read the @Preamble definition at the line POS is on.
Return the preamble as a string, or nil if none was found.

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @Preamble must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (parsebib--looking-at-goto-end (concat parsebib--entry-start "preamble[[:space:]]*[\(\{]"))
    (let ((beg (point)))
      (forward-char -1)
      (when (parsebib--match-paren-forward)
        (buffer-substring-no-properties beg (point))))))

(defun parsebib-read-entry (type &optional pos strings)
  "Read a BibTeX entry of type TYPE at the line POS is on.

TYPE should be a string and should not contain the @
sign.  The return value is the entry as an alist of (<field> .
<contents>) cons pairs, or nil if no entry was found.  In this
alist, the entry key is provided in the field \"=key=\" and the
entry type in the field \"=type=\".

POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the entry must start at the beginning of
the line POS is on.  If POS is nil, it defaults to point.

ENTRY should not be \"Comment\", \"Preamble\" or \"String\", but
is otherwise not limited to any set of possible entry types. If
so required, the calling function has to ensure that the entry
type is valid.

If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the entry's
fields."
  (unless (member-ignore-case type '("comment" "preamble" "string"))
    (when pos (goto-char pos))
    (beginning-of-line)
    (when (parsebib--looking-at-goto-end (concat parsebib--entry-start type "[[:space:]]*[\(\{]"))
      ;; find the end of the entry and the beginning of the entry key
      (let* ((limit (save-excursion
                      (backward-char)
                      (parsebib--match-paren-forward)
                      (point)))
             (beg (progn
                    (skip-chars-forward " \n\t\f") ; note the space!
                    (point)))
             (key (when (parsebib--looking-at-goto-end (concat "\\(" parsebib--key-regexp "\\)[ \t\n\f]*,") 1)
                    (buffer-substring-no-properties beg (point)))))
        (or key (setq key "")) ; if no key was found, we pretend it's empty and try to read the entry anyway
        (skip-chars-forward "^," limit) ; move to the comma after the entry key
        (let ((fields (cl-loop for field = (parsebib--find-bibtex-field limit strings)
                               while field collect field)))
          (push (cons "=type=" type) fields)
          (push (cons "=key=" key) fields)
          (nreverse fields))))))

(defun parsebib--find-bibtex-field (limit &optional strings)
  "Find the field after point.
Do not search beyond LIMIT (a buffer position).  Return a
cons (FIELD . VALUE), or nil if no field was found.

If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the field's
value."
  (skip-chars-forward "\"#%'(),={} \n\t\f" limit) ; move to the first char of the field name
  (unless (>= (point) limit)                      ; if we haven't reached the end of the entry
    (let ((beg (point)))
      (if (parsebib--looking-at-goto-end (concat "\\(" parsebib--bibtex-identifier "\\)[[:space:]]*=[[:space:]]*") 1)
          (let ((field-type (buffer-substring-no-properties beg (point))))
            (let ((field-contents (parsebib--parse-value limit strings)))
              (cons field-type field-contents)))))))

;;;;;;;;;;;;;;;;;;;;
;; high-level API ;;
;;;;;;;;;;;;;;;;;;;;

(defun parsebib-collect-strings (&optional hash expand-strings)
  "Collect all @String definitions in the current buffer.
Return value is a hash with the abbreviations as keys and the
expansions as values.  If HASH is a hash table with test function
`equal', it is used to store the @String definitions.  If
EXPAND-STRINGS is non-nil, @String expansions are expanded
themselves using the @String definitions already stored in HASH."
  (save-excursion
    (goto-char (point-min))
    (let* ((res (if (and (hash-table-p hash)
                         (eq 'equal (hash-table-test hash)))
                    hash
                  (make-hash-table :test #'equal)))
           string)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (when (cl-equalp item "string")
                 (setq string (parsebib-read-string nil (if expand-strings hash)))
                 (puthash (car string) (cdr string) res)))
      hash)))

(defun parsebib-collect-preambles ()
  "Collect all @Preamble definitions in the current buffer.
Return a list of strings, each string a separate @Preamble."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (when (cl-equalp item "preamble")
                 (push (parsebib-read-preamble) res)))
      (nreverse res))))

(defun parsebib-collect-comments ()
  "Collect all @Comment definitions in the current buffer.
Return a list of strings, each string a separate @Comment."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (when (cl-equalp item "comment")
                 (push (parsebib-read-comment) res)))
      (nreverse res))))

(defun parsebib-collect-entries (&optional hash strings)
  "Collect all entries is the current buffer.
Return value is a hash table containing the entries.  If HASH is
a hash table, with test function `equal', it is used to store the
entries.  If STRINGS is non-nil, it should be a hash table of
string definitions, which are used to expand abbreviations used
in the entries."
  (save-excursion
    (goto-char (point-min))
    (let* ((res (if (and (hash-table-p hash)
                         (eq 'equal (hash-table-test hash)))
                    hash
                  (make-hash-table :test #'equal)))
           entry)
      (cl-loop for entry-type = (parsebib-find-next-item)
               while entry-type do
               (unless (member-ignore-case entry-type '("preamble" "string" "comment"))
                 (setq entry (parsebib-read-entry entry-type nil strings))
                 (if entry
                     (puthash (cdr (assoc-string "=key=" entry)) entry res))))
      hash)))


(defun parsebib-find-bibtex-dialect ()
  "Find the BibTeX dialect of a file if one is set.
This function looks for a local value of the variable
`bibtex-dialect' in the local variable block at the end of the
file.  Return nil if no dialect is found."
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search t))
      (when (re-search-backward (concat parsebib--entry-start "comment") (- (point-max) 3000) t)
        (let ((comment (parsebib-read-comment)))
          (when (and comment
                     (string-match-p "\\`[ \n\t\r]*Local Variables:" comment)
                     (string-match-p "End:[ \n\t\r]*\\'" comment)
                     (string-match (concat "bibtex-dialect: " (regexp-opt (mapcar #'symbol-name bibtex-dialect-list) t)) comment))
            (intern (match-string 1 comment))))))))

(defun parsebib-parse-buffer (&optional entries-hash strings-hash expand-strings)
  "Parse the current buffer and return all BibTeX data.
Return list of five elements: a hash table with the entries, a
hash table with the @String definitions, a list of @Preamble
definitions, a list of @Comments and the BibTeX dialect, if
present in the file.

If ENTRIES-HASH is a hash table with test function `equal', it is
used to store the entries.  Any existing entries with identical
keys are overwritten.  Similarly, if STRINGS-HASH is a hash table
with test function `equal', the @String definitions are stored in
it.

If EXPAND-STRINGS is non-nil, abbreviations in the entries and
@String definitions are expanded using the @String definitions
already in STRINGS."
  (save-excursion
    (goto-char (point-min))
    (let ((entries (if (and (hash-table-p entries-hash)
                            (eq (hash-table-test entries-hash) 'equal))
                       entries-hash
                     (make-hash-table :test #'equal)))
          (strings (if (and (hash-table-p strings-hash)
                            (eq (hash-table-test strings-hash) 'equal))
                       strings-hash
                     (make-hash-table :test #'equal)))
          preambles comments)
      (cl-loop for item = (parsebib-find-next-item)
               while item do
               (cond
                ((cl-equalp item "string") ; `cl-equalp' compares strings case-insensitively.
                 (let ((string (parsebib-read-string nil (if expand-strings strings))))
                   (if string
                       (puthash (car string) (cdr string) strings))))
                ((cl-equalp item "preamble")
                 (push (parsebib-read-preamble) preambles))
                ((cl-equalp item "comment")
                 (push (parsebib-read-comment) comments))
                ((stringp item)
                 (let ((entry (parsebib-read-entry item nil (if expand-strings strings))))
                   (when entry
                     (puthash (cdr (assoc-string "=key=" entry)) entry entries))))))
      (list entries strings preambles comments (parsebib-find-bibtex-dialect)))))

(provide 'parsebib)

;;; parsebib.el ends here
