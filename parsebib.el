;;; parsebib.el --- A library for parsing bib files  -*- lexical-binding: t -*-

;; Copyright (c) 2014-2025 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2014
;; Version: 6.7
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

;; See the README for details.
;;
;; Acknowledgements:
;;
;; The code to clean up TeX markup in field values was contributed by Hugo
;; Heagren <hugo.heagren@kcl.ac.uk>; additional improvements were made by
;; <rahguzar@zohomail.eu>.

;;; Code:

(require 'cl-lib)
(eval-and-compile (unless (fboundp 'json-parse-buffer)
                    (require 'json)))
(defvar json-object-type)

(declare-function json-read "json.el")

(defvar bibtex-dialect)
(defvar bibtex-dialect-list)

(defvar parsebib-hashid-fields nil
  "List of fields used to create a hash id for each entry.
Hash ids can only be created for BibTeX/biblatex files.  The hash
id is stored in the entry in the special field `=hashid='.")

;; Regexes describing BibTeX identifiers and keys.  Note that while $ ^ &
;; are valid in BibTeX keys, they may nonetheless be problematic, because
;; they are special for TeX.  Which characters are allowed in keys and
;; identifiers differs depending on context.  The StackExchange answer at
;; https://tex.stackexchange.com/questions/96454/using-bibtex-keys-containing-parentheses-with-biber
;; says that Biber uses a library for parsing .bib files (btparse) that
;; disallows the following characters in keys;
;;
;; " # % ' ( ) , = { }
;;
;; Note that parentheses are allowed by BibTex, though, so I won't exclude
;; them here.
;;
;; Also, keys and identifiers (field and @String names) are distinguished,
;; though I'm not sure that is correct (or even desirable).  I'll keep it
;; that way until someone complains, though.

(defconst parsebib--bibtex-identifier "[^\"@\\#%',={}() \t\n\f]+" "Regexp describing a licit BibTeX identifier.")
(defconst parsebib--bibtex-key-regexp "[^\"#%',={} \t\n\f]+" "Regexp describing a licit BibTeX key.")
(defconst parsebib--bibtex-entry-start "^[ \t]*@" "Regexp describing the start of an entry.")

(defvar parsebib-postprocessing-excluded-fields '("file"
                                                  "url"
                                                  "doi")
  "List of fields that should not be post-processed.")

;; Cleaning up TeX code is very slow, so we restrict it to those fields for
;; which it makes sense.
(defvar parsebib-replace-TeX-fields '("author" "editor" "title")
  "List of fields in which TeX code should be cleaned up.")

(defvar parsebib--biblatex-inheritances '(;; Source                        Target
                                          ("all"                           "all"
                                           (("ids"                         . none)
                                            ("crossref"                    . none)
                                            ("xref"                        . none)
                                            ("entryset"                    . none)
                                            ("entrysubtype"                . none)
                                            ("execute"                     . none)
                                            ("label"                       . none)
                                            ("options"                     . none)
                                            ("presort"                     . none)
                                            ("related"                     . none)
                                            ("relatedoptions"              . none)
                                            ("relatedstring"               . none)
                                            ("relatedtype"                 . none)
                                            ("shorthand"                   . none)
                                            ("shorthandintro"              . none)
                                            ("sortkey"                     . none)))

                                          ;; Source                        Target
                                          ("mvbook, book"                  "inbook, bookinbook, suppbook"
                                           (("author"                      . "author")
                                            ("author"                      . "bookauthor")))

                                          ;; Source                        Target
                                          ("mvbook"                        "book, inbook, bookinbook, suppbook"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("mvcollection, mvreference"     "collection, reference, incollection, inreference, suppcollection"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("mvproceedings"                 "proceedings, inproceedings"
                                           (("title"                       . "maintitle")
                                            ("subtitle"                    . "mainsubtitle")
                                            ("titleaddon"                  . "maintitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("book"                          "inbook, bookinbook, suppbook"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("collection, reference"         "incollection, inreference, suppcollection"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("proceedings"                   "inproceedings"
                                           (("title"                       . "booktitle")
                                            ("subtitle"                    . "booksubtitle")
                                            ("titleaddon"                  . "booktitleaddon")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none)))

                                          ;; Source                        Target
                                          ("periodical"                    "article, suppperiodical"
                                           (("title"                       . "journaltitle")
                                            ("subtitle"                    . "journalsubtitle")
                                            ("shorttitle"                  . none)
                                            ("sorttitle"                   . none)
                                            ("indextitle"                  . none)
                                            ("indexsorttitle"              . none))))

  "Inheritance scheme for BibLaTeX cross-referencing.
Inheritances are specified for pairs of source and target entry
type, where the target is the cross-referencing entry and the
source the cross-referenced entry.  Each pair specifies the
fields in the source and the fields in the target that they
correspond with.

Inheritances valid for all entry types are defined by specifying
the entry type as \"all\".  The entry type may also be a
comma-separated list of entry types.

If no inheritance rule is set up for a given entry type+field
combination, the field inherits from the same-name field in the
cross-referenced entry.  If no inheritance should take place, the
target field is set to the symbol `none'.")

;;;; BibTeX / biblatex parser

;;; Parser primitives
;;
;; The parser is divided into a set of primitives, which do the actual
;; reading, and a set of grammar rules, which describe the syntax of a
;; BibTeX file.
;;
;; A few things to keep in mind:
;;
;; - The primitives are BibTeX-agnostic. They read specific chunks of the
;;   source and return them.
;;
;; - The primitives can be parameterised; that is, the exact text that they
;;   read may depend on arguments passed to them. The grammar rules do not
;;   have any parameters.
;;
;; - The primitives are responsible for skipping whitespace.

(define-error 'parsebib-error "[Parsebib error]" 'error)

(defun parsebib--skip-whitespace ()
  "Skip whitespace."
  (skip-chars-forward " \n\r\t\f\v"))

(defun parsebib--char (chars &optional noerror)
  "Read the character at point.
CHARS is a list of characters.  If the character at point matches
a character in CHARS, return it and move point, otherwise signal
an error, unless NOERROR is non-nil, in which case return nil."
  (parsebib--skip-whitespace)
  (if (memq (char-after) chars)
      (prog1
          (char-after)
        (forward-char 1))
    (unless noerror
      (signal 'parsebib-error (list (point)
                                    "Invalid character `%c'"
                                    (following-char))))))

(defun parsebib--keyword (keywords &optional noerror)
  "Read the keyword following point.
KEYWORDS is a list of allowed keywords.  If the text following
point matches one of KEYWORDS (case-insensitively), return it and
move point.  Otherwise signal an error, unless NOERROR is
non-nil, in which case return nil."
  (parsebib--skip-whitespace)
  (let ((case-fold-search t))
    (if (looking-at (regexp-opt keywords))
        (let ((keyword (match-string-no-properties 0)))
          (progn
            (goto-char (match-end 0))
            keyword))
      (unless noerror
        (signal 'parsebib-error (list (point)
                                      "Invalid keyword %s"
                                      keywords
                                      (char-after)))))))

(defun parsebib--symbol (regexp &optional noerror)
  "Read a symbol and return it.
REGEXP is a regular expression describing a licit symbol.  If a
symbol is found, return it.  Otherwise signal an error, unless
NOERROR is non-nil, in which case return nil."
  (parsebib--skip-whitespace)
  (if (looking-at regexp)
      (progn
        (goto-char (match-end 0))
        (match-string-no-properties 0))
    (unless noerror
      (signal 'parsebib-error (list (point) "Illegal identifier")))))

(defun parsebib--seq-delim (open close esc)
  "Read a delimited sequence.
A delimited sequence is a sequence delimited by OPEN and CLOSE
characters, which must be different (e.g., any kind of
parentheses).  ESC is an escape character that can be used to
escape OPEN and CLOSE inside the sequence.  OPEN and CLOSE can
appear in the sequence unescaped as long as they are
balanced.  (In other words, the sequence can contain nested
sequences)."
  (parsebib--skip-whitespace)
  (let ((beg (point))
        (n-braces 1)
        (skip-chars (format "^%c%c" open close)))
    (parsebib--char (list open))
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
      (ignore-error end-of-buffer (forward-char 1)))
    (if (= n-braces 0)
        (buffer-substring-no-properties beg (point))
      (signal 'parsebib-error (list beg
                                    "Opening %c has no closing %c"
                                    open
                                    close)))))

(defun parsebib--string (delim esc)
  "Read a string delimited by DELIM.
A string is a delimited sequence where the opening and closing
delimiters are identical, e.g., \"...\".  ESC is the escape
character."
  (parsebib--skip-whitespace)
  (let ((beg (point))
        (continue t)
        (skip-chars (format "^%c" delim)))
    (parsebib--char (list delim))
    (while (and continue
                (not (eobp)))
      (skip-chars-forward skip-chars)
      (unless (eq (char-before) esc)
        (setq continue nil))
      (forward-char 1))
    (if (not continue)
        (buffer-substring-no-properties beg (point))
      (signal 'parsebib-error (list beg
                                    "Opening %c has no closing %c"
                                    delim
                                    delim)))))

(defun parsebib--comment-line ()
  "Read a single-line comment and return it."
  (prog1 (buffer-substring-no-properties (point) (pos-eol))
    (forward-line 1)))

(defun parsebib--match (rules &optional noerror)
  "Check if a rule in RULES matches at point.
Apply the first rule that matches and return the result.  If no
rule matches, signal an error, unless NOERROR is non-nil, in
which case return nil.

RULES is a list of symbols, each naming a parsing rule."
  (parsebib--skip-whitespace)
  (let ((start-pos (point))
        last-error)
    (catch 'success
      (dolist (rule rules)
        (condition-case err
            (let ((res (funcall rule)))
              (throw 'success res))
          (parsebib-error
           (goto-char start-pos)
           (setq last-error err))))
      (unless noerror
        (signal (car last-error) (cdr last-error))))))

;;; Parser rules

;; Basic building blocks

(defun parsebib--text ()
  "Parse text.
Text is anything that is between braces or double quotes that
should be read literally."
  (parsebib--match '(parsebib--braced-text
                     parsebib--quoted-text)))

(defun parsebib--braced-text ()
  "Parse text in curly braces."
  (parsebib--seq-delim ?\{ ?\} ?\\))

(defun parsebib--quoted-text ()
  "Parse text in double quotes."
  (parsebib--string ?\" ?\\))

(defun parsebib--key ()
  "Parse a BibTeX key."
  (parsebib--symbol parsebib--bibtex-key-regexp))

(defun parsebib--identifier ()
  "Parse a BibTeX identifier."
  (parsebib--symbol parsebib--bibtex-identifier))

(defun parsebib--value ()
  "Parse a BibTeX value.
A value is one component of a composed value (see
`parsebib--composed-value') and can either be a piece of quoted
text (i.e., text in double quotes or braces) or a @String
abbreviation."
  (or (parsebib--match '(parsebib--text
                         parsebib--identifier)
                       :noerror)
      (signal 'parsebib-error (list (point) "Expected {, \" or identifier"))))

(defun parsebib--composed-value ()
  "Parse a BibTeX composed field value.
A composed value consists of one or more values concatenated
using the character `#'.  They typically appear after an equal
sign as field values and in @String definitions as expansions."
  (let ((val (list (parsebib--value))))
    (while (and (parsebib--char '(?#) :noerror)
                (not (eobp)))
      (push (parsebib--value) val))
    (nreverse val)))

(defun parsebib--assignment ()
  "Parse a BibTeX assignment.
An assignment is the combination of an identifier, an equal sign
and a composed value.  A @String definition has exactly one
assignment, an entry has a potentially unlimited number."
  (if-let* ((id (parsebib--identifier))
            (_ (parsebib--char '(?=)))
            (val (parsebib--composed-value)))
      (cons id val)
    (signal 'parsebib-error (list (point) "Malformed key=value assignment"))))

(defun parsebib--fields ()
  "Parse a set of BibTeX assignments.
A set of assignments makes up the body of an entry."
  (let ((fields (list (parsebib--assignment))))
    (while (and (parsebib--char '(?,) :noerror)
                (not (eobp)))
      ;; There may be a comma after the final field of an entry. If that
      ;; happens, reading another assignment will fail, so we capture the
      ;; error here.
      (ignore-error parsebib-error
        (push (parsebib--assignment) fields)))
    fields))

;; BibTeX items

(defun parsebib--@comment ()
  "Parse a @Comment.
Return the contents of the @Comment as a string."
  (parsebib--char '(?@))
  (parsebib--keyword '("comment"))
  (or (parsebib--match '(parsebib--text
                         parsebib--comment-line)
                       :noerror)
      (signal 'parsebib-error (list (point) "Malformed @Comment"))))

(defun parsebib--@preamble ()
  "Parse a @Preamble.
Return the contents of the @Preamble as a string."
  (parsebib--char '(?@))
  (parsebib--keyword '("preamble"))
  (or (parsebib--text)
      (signal 'parsebib-error (list (point) "Malformed @Preamble"))))

(defun parsebib--@string ()
  "Parse an @String definition.
Return the definition as a cons cell of the abbreviation and a
composed value as a list."
  (if-let* ((_ (parsebib--char '(?@)))
            (_ (parsebib--keyword '("string")))
            (open (parsebib--char '(?\{ ?\( )))
            (definition (parsebib--assignment))
            (_ (parsebib--char (alist-get open '((?\{ ?\}) (?\( ?\)))))))
      definition
    (signal 'parsebib-error (list (point) "Malformed @String definition"))))

(defun parsebib--@entry ()
  "Parse a BibTeX database entry.
Return the entry as an alist of <field . value> pairs, where
<field> is a string and <value> is a list of strings."
  (if-let* ((_ (parsebib--char '(?@)))
            (type (parsebib--identifier))
            (open (parsebib--char '(?\{ ?\( )))
            (key (parsebib--key))
            (_ (parsebib--char '(?,)))
            (fields (parsebib--fields))
            (_ (parsebib--char (alist-get open '((?\{ ?\}) (?\( ?\)))))))
      (progn (push (cons "=type=" (list type)) fields)
             (push (cons "=key=" (list key)) fields)
             fields)
    (signal 'parsebib-error (list (point) "Malformed entry definition"))))

;;;; Low-level BibTeX/biblatex API

(defun parsebib-find-next-item ()
  "Find the first (potential) BibTeX item following point.
This function simply searches for an @ at the start of a line,
possibly preceded by spaces or tabs, followed by a string of
characters as defined by `parsebib--bibtex-identifier'.

If an item is found, position point at the start of the line and
return the name of the item as a string, either \"Comment\",
\"Preamble\" or \"String\", or the entry type (without the @).
If no item is found, move point to the end of the buffer."
  (when (re-search-forward parsebib--bibtex-entry-start nil 0)
    (if (looking-at (concat "\\(" parsebib--bibtex-identifier "\\)" "[[:space:]]*[\(\{]?"))
        (prog1
            (match-string-no-properties 1)
          (goto-char (pos-bol)))
      (signal 'parsebib-error (list (point) "Search for BibTeX entry failed")))))

(defun parsebib--get-hashid-string (fields)
  "Create a string from the contents of FIELDS to compute a hash id."
  (cl-loop for field in parsebib-hashid-fields
           collect (or
                    ;; Remove braces {}.
                    (replace-regexp-in-string "^{\\|}$" "" (cdr (assoc-string field fields 'case-fold)))
                    "")
           into hashid-fields
           finally return (mapconcat #'identity hashid-fields "")))

(defun parsebib-read-entry (&optional fields strings replace-TeX)
  "Read a BibTeX entry starting at point.
Point should be positioned before the `@'-character that starts
the entry, with possibly whitespace intervening.  Return an alist
of (<field> .  <contents>) conses, or nil if no entry was found.
The returned alist provides the entry key in the field \"=key=\"
and the entry type in the field \"=type=\".

If `parsebib-hashid-fields' is non-nil, a hash ID is added in the
field \"=hashid=\".  The hash is computed on the basis of the
contents of the fields listed in `parsebib-hashid-fields' using
the function `secure-hash' and the `sha256' algorithm.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored.
Case is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned.  Note that if FIELDS is
non-nil, it should contain the field names \"=key=\" and
\"=type=\".

STRINGS and REPLACE-TEX are used to post-process field values.
See the function `parsebib--post-process' for details."
  (let ((entry (parsebib--@entry)))
    (when fields
      (setq entry (seq-filter (lambda (field)
                                (member-ignore-case (car field) fields))
                              entry)))
    (setq entry (mapcar (lambda (field)
                          (parsebib--post-process field strings replace-TeX))
                        entry))
    (when parsebib-hashid-fields
      (push (cons "=hashid=" (secure-hash 'sha256 (parsebib--get-hashid-string fields))) entry))
    entry))

(defun parsebib-read-string (&optional strings)
  "Read the @String definition beginning at point.
Return the definition as a cons cell (<abbrev> . <expansion>).

If STRINGS is provided, it should be a hash table with @String
abbreviations, which are used to expand abbreviations in the
string's expansion."
  (let* ((definition (parsebib--@string))
         (abbrev (car definition))
         (expansion (cdr definition)))
    (setq expansion (if strings
                        (string-join (parsebib--post-process-strings expansion strings t))
                      (string-join expansion " # ")))
    (cons abbrev expansion)))

(defalias 'parsebib-read-preamble 'parsebib--@preamble)
(defalias 'parsebib-read-comment 'parsebib--@comment)

;;;;; Post-processing stuff

(defun parsebib--post-process (field strings replace-TeX)
  "Post-process FIELD.
FIELD is a cons cell consisting of the field name and the field
value.  The field value is a list of strings.

If STRINGS is provided, it should be a hash table with string
definitions.  @String abbreviations in field values are then
expanded using these definitions.  In addition, field values are
unquoted, newlines are removed and sequences of whitespace are
collapsed into a single space.

If REPLACE-TEX is non-nil, TeX markup is cleaned up.  See the
variable `parsebib-TeX-markup-replace-alist' for details.

No post-processing is applied to fields listed in
`parsebib-postprocessing-excluded-fields', with the exception of
unquoting, which is always applied if STRINGS is non-nil.

Finally, the strings in the field value are concatenated.  Return
value is a cons cell of field name and field value, the value now
being a single string."
  (let* ((name (car field))
         (value (cdr field))
         (post-process (not (member-ignore-case name parsebib-postprocessing-excluded-fields)))
         (replace-TeX (and replace-TeX (member-ignore-case name parsebib-replace-TeX-fields))))
    (setq value (if strings
                    (string-join (parsebib--post-process-strings value strings post-process))
                  (string-join value " # ")))
    (when (and replace-TeX post-process)
      (setq value (parsebib-clean-TeX-markup value)))
    (cons name value)))

(defun parsebib--post-process-strings (strings abbrevs post-process)
  "Post-process the strings in STRINGS.
STRINGS is a list of strings, ABBREVS a hash table with @String
definitions.  Post-processing involves three changes: First,
sequences of whitespace are collapsed into a single space.
Second, if a string has an expansion in ABBREVS, it is replaced
with the expansion.  Both these changes are only applied if
POST-PROCESS is non-nil.  Lastly, if the string is enclosed in
braces {} or double -quotes \"\", these are removed."
  (mapcar (lambda (str)
            (when post-process
              (setq str (replace-regexp-in-string "[[:space:]\t\n\f]+" " " str)))
            (cond
             ((and post-process
                   (gethash str abbrevs)))
             ((string-match "\\`[\"{]\\(.*?\\)[\"}]\\'" str)
              (match-string 1 str))
             (t str)))
          strings))

(defun parsebib-expand-xrefs (entries inheritance)
  "Expand cross-referencing items in ENTRIES.
BibTeX entries in ENTRIES that have a `crossref' field are
expanded with the fields in the cross-referenced entry.  ENTRIES
is a hash table with entries.  This hash table is updated with
the new fields.  The return value of this function is always nil.

INHERITANCE indicates the inheritance schema.  It can be a symbol
`BibTeX' or `biblatex', or it can be an explicit inheritance
schema.  See the variable `parsebib--biblatex-inheritances' for
details on the structure of such an inheritance schema."
  (maphash (lambda (key fields)
             (let ((xref (cdr (assoc-string "crossref" fields))))
               (when xref
                 (if (string-match-p (concat "\\b[\"{]" parsebib--bibtex-key-regexp "[\"}]\\b") xref)
                     (setq xref (substring xref 1 -1)))
                 (let* ((source (gethash xref entries))
                        (updated-entry (parsebib--get-xref-fields fields source inheritance)))
                   (when updated-entry
                     (puthash key updated-entry entries))))))
           entries))

(defun parsebib--get-xref-fields (target-entry source-entry inheritance)
  "Return TARGET-ENTRY supplemented with fields inherited from SOURCE-ENTRY.
TARGET-ENTRY and SOURCE-ENTRY are entry alists.  Fields in
SOURCE-ENTRY for which TARGET-ENTRY has no value are added to
TARGET-ENTRY.  Return value is the modified TARGET-ENTRY.

INHERITANCE is an inheritance schema.  It can either be one of
the symbols `BibTeX' or `biblatex', or it can be an explicit
inheritance schema.  See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema."
  (when (and target-entry source-entry)
    (when (eq inheritance 'biblatex)
      (setq inheritance parsebib--biblatex-inheritances))
    (let* ((source-type (concat "\\b" (cdr (assoc-string "=type=" source-entry)) "\\b"))
           (target-type (concat "\\b" (cdr (assoc-string "=type=" target-entry)) "\\b"))
           (for-all-types (nth 2 (assoc-string "all" inheritance)))
           (inheritable-fields
            (unless (eq inheritance 'BibTeX)
              (append
               (apply #'append (mapcar #'cl-third
                                       (cl-remove-if-not
                                        (lambda (elem)
                                          (and (string-match-p source-type (nth 0 elem))
                                               (string-match-p target-type (nth 1 elem))))
                                        inheritance)))
               for-all-types)))
           (new-fields (mapcan (lambda (field)
                                 (let ((target-field (parsebib--get-target-field (car field) inheritable-fields)))
                                   (if (and target-field
                                            (not (assoc-string target-field target-entry 'case-fold)))
                                       (list (cons target-field (cdr field))))))
                               source-entry)))
      (append target-entry new-fields))))

(defun parsebib--get-target-field (source-field inheritances)
  "Return the target field for inheritance from SOURCE-FIELD.
Inheritance is determined by INHERITANCES, which is an alist of
source/target pairs.  If no inheritance should take place for
SOURCE-FIELD, the target in the relevant item in INHERITANCES is
the symbol `none'.  If there is no item for SOURCE-FIELD in
INHERITANCES, SOURCE-FIELD is returned.  Note that it is valid
for INHERITANCES to be nil."
  ;; Note: the argument INHERITANCES differs from the INHERITANCE argument in
  ;; the previous two functions.  It is a simple alist of (source-field
  ;; . target-field) pairs.
  (let ((target-field (cdr (assoc-string source-field inheritances 'case-fold))))
    (cond
     ((null target-field)
      source-field)
     ((eq target-field 'none)
      nil)
     (t target-field))))

;;;; Clean up TeX markup

(defvar parsebib-TeX-cleanup-target 'display
  "Target for `parsebib-clean-TeX-markup'.
This variable affects the output of the functions that convert
LaTeX font commands \\textbf, \\textit, and \\emph.  Its value
should be one of the symbols `display', `markdown' `org' or
`plain'.  Any other value is treated as a synonym for `plain'.
See `parsebib--convert-tex-italics' and
`parsebib--convert-tex-bold' for details.")

(defun parsebib--convert-tex-italics (str)
  "Return STR converted to italic face.
Depending on the value of `parsebib-TeX-cleanup-target', add a
face property `italic' to STR, or return it with Markdown or Org
markup for italic text."
  (pcase parsebib-TeX-cleanup-target
    ('display (propertize str 'face 'italic))
    ('markdown (concat "*" str "*"))
    ('org (concat "/" str "/"))
    (_ str)))

(defun parsebib--convert-tex-bold (str)
  "Return STR converted to bold face.
Depending on the value of `parsebib-TeX-cleanup-target', add a
face property `bold' to STR, or return it with Markdown or Org
markup for bold text."
  (pcase parsebib-TeX-cleanup-target
    ('display (propertize str 'face 'bold))
    ('markdown (concat "**" str "**"))
    ('org (concat "*" str "*"))
    (_ str)))

(defvar parsebib-TeX-command-replacement-alist
  '(("ddag"               . "\N{DOUBLE DAGGER}")
    ("textdaggerdbl"      . "\N{DOUBLE DAGGER}")
    ("dag"                . "\N{DAGGER}")
    ("textdagger"         . "\N{DAGGER}")
    ("textpertenthousand" . "\N{PER TEN THOUSAND SIGN}")
    ("textperthousand"    . "\N{PER MILLE SIGN}")
    ("textquestiondown"   . "\N{INVERTED QUESTION MARK}")
    ("P"                  . "\N{PILCROW SIGN}")
    ("textdollar"         . "$")
    ("S"                  . "\N{SECTION SIGN}")
    ("ldots"              . "\N{HORIZONTAL ELLIPSIS}")
    ("dots"               . "\N{HORIZONTAL ELLIPSIS}")
    ("textellipsis"       . "\N{HORIZONTAL ELLIPSIS}")
    ("textemdash"         . "\N{EM DASH}")
    ("textendash"         . "\N{EN DASH}")
    ("textbar"            . "|")

    ;; Non-ASCII Letters (Excluding Accented Letters)
    ("AA" . "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}")
    ("AE" . "\N{LATIN CAPITAL LETTER AE}")
    ("DH" . "\N{LATIN CAPITAL LETTER ETH}")
    ("DJ" . "\N{LATIN CAPITAL LETTER ETH}")
    ("L"  . "\N{LATIN CAPITAL LETTER L WITH STROKE}")
    ("SS" . "\N{LATIN CAPITAL LETTER SHARP S}")
    ("NG" . "\N{LATIN CAPITAL LETTER ENG}")
    ("OE" . "\N{LATIN CAPITAL LIGATURE OE}")
    ("O"  . "\N{LATIN CAPITAL LETTER O WITH STROKE}")
    ("TH" . "\N{LATIN CAPITAL LETTER THORN}")

    ("aa" . "\N{LATIN SMALL LETTER A WITH RING ABOVE}")
    ("ae" . "\N{LATIN SMALL LETTER AE}")
    ("dh" . "\N{LATIN SMALL LETTER ETH}")
    ("dj" . "\N{LATIN SMALL LETTER ETH}")
    ("l"  . "\N{LATIN SMALL LETTER L WITH STROKE}")
    ("ss" . "\N{LATIN SMALL LETTER SHARP S}")
    ("ng" . "\N{LATIN SMALL LETTER ENG}")
    ("oe" . "\N{LATIN SMALL LIGATURE OE}")
    ("o"  . "\N{LATIN SMALL LETTER O WITH STROKE}")
    ("th" . "\N{LATIN SMALL LETTER THORN}")

    ("ij" . "ij")
    ("i"  . "\N{LATIN SMALL LETTER DOTLESS I}")
    ("j"  . "\N{LATIN SMALL LETTER DOTLESS J}")

    ;; Formatting Commands
    ("textit" . parsebib--convert-tex-italics)
    ("emph"   . parsebib--convert-tex-italics)
    ("textbf" . parsebib--convert-tex-bold)
    ("textsc" . upcase))
  "An alist of <command>-<replacement> pairs for LaTeX commands.
<command> is the name of a TeX or LaTeX command (without
backslash), <replacement> is the string with which it is
replaced.

<replacement> can also be a function of one argument.  In this
case, <command> must take at least one obligatory argument, which
is passed as the first argument of the replacement function.  The
return value of this function is used as the replacement string
for <command>.

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-accent-replacement-alist
  '(("\"" . "\N{COMBINING DIAERESIS}")
    ("'"  . "\N{COMBINING ACUTE ACCENT}")
    ("."  . "\N{COMBINING DOT ABOVE}")
    ("="  . "\N{COMBINING MACRON}")
    ("^"  . "\N{COMBINING CIRCUMFLEX ACCENT}")
    ("`"  . "\N{COMBINING GRAVE ACCENT}")
    ("b"  . "\N{COMBINING MACRON BELOW}")
    ("c"  . "\N{COMBINING CEDILLA}")
    ("d"  . "\N{COMBINING DOT BELOW}")
    ("H"  . "\N{COMBINING DOUBLE ACUTE ACCENT}")
    ("k"  . "\N{COMBINING OGONEK}")
    ("U"  . "\N{COMBINING DOUBLE VERTICAL LINE ABOVE}")
    ("u"  . "\N{COMBINING BREVE}")
    ("v"  . "\N{COMBINING CARON}")
    ("~"  . "\N{COMBINING TILDE}")
    ("|"  . "\N{COMBINING COMMA ABOVE}")
    ("f"  . "\N{COMBINING INVERTED BREVE}")
    ("G"  . "\N{COMBINING DOUBLE GRAVE ACCENT}")
    ("h"  . "\N{COMBINING HOOK ABOVE}")
    ("C"  . "\N{COMBINING DOUBLE GRAVE ACCENT}")
    ("r"  . "\N{COMBINING RING ABOVE}") )
  "Alist of <command>-<accent> pairs for LaTeX diacritics.
<command> is the name of a TeX or LaTeX command (without
backslash), <accent> is the Unicode combining character for the
diacritic that <command> generates.  Both <command> and <accent>
must be strings.

The replacement string for <command> is composed of its
obligatory argument (usually a single character) and the
combining diacritic.

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-literal-replacement-alist
  ;; LaTeX2 Escapable "Special" Characters
  `(("\\%" . "%") ("\\&" . "&") ("\\#" . "#") ("\\$" . "$")
    ;; Quotes
    ("``" . "\N{LEFT DOUBLE QUOTATION MARK}")
    ("`"  . "\N{LEFT SINGLE QUOTATION MARK}")
    ("''" . "\N{RIGHT DOUBLE QUOTATION MARK}")
    ("'"  . "\N{RIGHT SINGLE QUOTATION MARK}")
    ;; Dashes
    ("---" . "\N{EM DASH}")
    ("--"  . "\N{EN DASH}")
    ;; Remove all remaining {braces}
    ("{" . "") ("}" . ""))
  "Alist of <literal>-<replacement> pairs.  Both are strings.
This variable contains characters that are special in LaTeX and
single-character, non-ASCII LaTeX commands.

Note that adding pairs to this variable has no effect unless
`parsebib-TeX-markup-replacement-alist' is adjusted accordingly.
For example, after adding a <literal>-<replacement> pair, the
following code will ensure that <literal> gets replaced with
<replacement>.

  (cl-callf (lambda (regex) (rx (or <literal> (regexp regex))))
     (alist-get (quote parsebib--TeX-replace-literal)
                parsebib-TeX-markup-replacement-alist))

See `parsebib-TeX-markup-replacement-alist' and the function
`parsebib-clean-TeX-markup' to see how this variable is used.")

(defvar parsebib-TeX-markup-replacement-alist
  `((parsebib--TeX-replace-command-or-accent
     ;; This regexp matches any latex command i.e. anything that
     ;; starts with a backslash. The name of the command which
     ;; is either a string of alphabetic characters or a single
     ;; non-alphabetic character is captured by group 1. The command
     ;; can have a mandatory argument enclosed by braces which is
     ;; captured by group 2. If the command has no arguments in
     ;; brackets or braces, the first non-white space letter after
     ;; the command is captured in group 3. This is to be able to deal
     ;; with accents.
     ;; Note that the capturing of arguments in braces is imperfect,
     ;; because doing it properly requires sexp parsing. It will fail
     ;; for cases like \command{\anothercommand{an arg}some text}.
     . ,(rx "\\" (group-n 1 (or (1+ letter) nonl))
            (: (* blank) (opt (or (: (* (: "[" (* (not (any "]"))) "]"))
                                     "{" (group-n 2 (0+ (not (any "}")))) (opt "}"))
                                  (group-n 3 letter))))))
    (parsebib--TeX-replace-literal
     . ,(rx (or (regexp (regexp-opt (mapcar #'car parsebib-TeX-literal-replacement-alist)))
                (1+ blank)))))
  "Alist of replacements and strings for TeX markup.
This is used in `parsebib-clean-TeX-markup' to make TeX markup more
suitable for display.  Each item in the list consists of a replacement
and a regexp.  The replacement can be a string (which will
simply replace the match) or a function (the match will be
replaced by the result of calling the function on the match
string).  Earlier elements are evaluated before later ones, so if
one string is a subpattern of another, the second must appear
later (e.g. \"''\" is before \"'\").

For the common cases of replacing a LaTeX command or a literal
it is faster to use `parsebib-TeX-command-replacement-alist'
and `parsebib-TeX-literal-replacement-alist' respectively.")

(defun parsebib--TeX-replace-command-or-accent (string)
  "Return the replacement text for the command or accent matched by STRING."
  (let* ((cmd (match-string 1 string))
         ;; bar is the argument in braces.
         (bar (match-string 2 string))
         ;; If there is no argument in braces, consider the letter after
         ;; the command as the argument. Clean this argument.
         (arg (parsebib-clean-TeX-markup (or (if bar bar (match-string 3 string)) "")))
         ;; Check if the cmd is an accent that needs to be replaced
         ;; and get its replacement.
         (acc (alist-get cmd parsebib-TeX-accent-replacement-alist nil nil #'equal))
         ;; If it is not an accent, check if it is a command that needs to be replaced
         ;; and get the replacement.
         (rep (or acc (alist-get cmd parsebib-TeX-command-replacement-alist nil nil #'equal))))
    (cond
     ;; If replacement is a function call it with the argument.
     ((functionp rep) (funcall rep arg))
     ;; Otherwise combine the replacement with the argument. The order of combination
     ;; depends on whether the command is an accent or not.
     (rep (if acc (concat arg rep) (concat rep arg)))
     ;; Now we handle the fallback cases. If there is a braced argument but no
     ;; replacement for the command was found, consider the replacement to be
     ;; empty.
     ((and bar (not (equal "" bar))) bar)
     ;; Otherwise clean any optional arguments by discarding them.
     (t (replace-regexp-in-string (rx "[" (* (not (any "]"))) "]") "" string t t)))))

(defun parsebib--TeX-replace-literal (string)
  "Look up the replacement text for literal STRING."
  (or (alist-get string parsebib-TeX-literal-replacement-alist nil nil #'equal)
      " "))

(defun parsebib-clean-TeX-markup (string)
  "Return STRING without TeX markup.
Any substring matching the car of a cell in
`parsebib-TeX-markup-replace-alist' is replaced with the
corresponding cdr (if the cdr is a string), or with the result of
calling the cdr on the match (if it is a function)."
  (let ((case-fold-search nil))
    (cl-loop for (replacement . pattern) in parsebib-TeX-markup-replacement-alist
             do (setq string (replace-regexp-in-string
                              pattern replacement string
                              t t))
             finally return string)))

;;;; High-level BibTeX/biblatex API

(defun parsebib-collect-preambles ()
  "Collect all @Preamble definitions in the current buffer.
Return a list of strings, each string a separate @Preamble."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while (and item
                          (cl-equalp item "preamble"))
               do (push (parsebib--@preamble) res))
      (nreverse res))))

(defun parsebib-collect-comments ()
  "Collect all @Comment definitions in the current buffer.
Return a list of strings, each string a separate @Comment."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (cl-loop for item = (parsebib-find-next-item)
               while (and item
                          (cl-equalp item "comment"))
               do (push (parsebib--@comment) res))
      (nreverse (delq nil res)))))

(cl-defun parsebib-collect-strings (&key strings expand-strings)
  "Collect all @String definitions in the current buffer.
Return value is a hash with the abbreviations as keys and the
expansions as values.  If STRINGS is a hash table with test
function `equal', it is used to store the @String definitions.
If EXPAND-STRINGS is non-nil, @String expansions are expanded
themselves using the @String definitions already stored in
STRINGS."
  (or (and (hash-table-p strings)
           (eq 'equal (hash-table-test strings)))
      (setq strings (make-hash-table :test #'equal)))
  (save-excursion
    (goto-char (point-min))
    (cl-loop with string = nil
             for item = (parsebib-find-next-item)
             while (and item
                        (cl-equalp item "string"))
             do
             (setq string (parsebib-read-string (if expand-strings strings)))
             (puthash (car string) (cdr string) strings))
    strings))

(cl-defun parsebib-collect-bib-entries (&key entries strings inheritance fields)
  "Collect all BibTeX / biblatex entries in the current buffer.
Return value is a hash table containing the entries.  If ENTRIES
is a hash table with test function `equal', it is used to store
the entries collected in the buffer.  Note that ENTRIES does not
have to be empty.  It may contain entries from a previous parse.

If STRINGS is non-nil, it should be a hash table of string
definitions, which are used to expand abbreviations used in the
entries.  In addition, if STRINGS is set, sequences of whitespace
in field values are collapsed into a single space, field values
are unquoted (i.e., the double quotes or braces around them are
removed), and TeX markup is prettified (see
`parsebib-clean-TeX-markup' for details).  Note that @String
expansion, collapsing of whitespace and prettifying TeX markup
are not applied to fields listed in
`parsebib-postprocessing-excluded-fields', but unquoting is.

If INHERITANCE is non-nil, cross-references in the entries are
resolved: if the crossref field of an entry points to an entry
already in ENTRIES (which includes the entries that appear
earlier in the buffer), the fields of the latter that do not occur
in the entry are added to it.  INHERITANCE indicates the
inheritance schema used for determining which fields inherit from
which fields.  It can be a symbol `BibTeX' or `biblatex', or it
can be an explicit inheritance schema.  (See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema.)  It can also be the symbol t, in
which case the local variable block is checked for a
dialect (using the variable `bibtex-dialect'), or, if no such
local variable is found, the value of the variable
`bibtex-dialect'.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned."
  (or (and (hash-table-p entries)
           (eq 'equal (hash-table-test entries)))
      (setq entries (make-hash-table :test #'equal)))
  (if (eq inheritance t)
      (setq inheritance (or (parsebib-find-bibtex-dialect)
                            (and (boundp 'bibtex-dialect) bibtex-dialect)
                            'BibTeX)))
  ;; Ensure =key= and =type= are in `fields'.
  (if fields
      (setq fields (append (list "=key=" "=type=" fields))))
  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (cl-loop with entry = nil
                 for entry-type = (parsebib-find-next-item)
                 while entry-type do
                 (unless (member-ignore-case entry-type '("preamble" "string" "comment"))
                   (setq entry (parsebib-read-entry fields strings (not (null strings))))
                   (if entry
                       (puthash (cdr (assoc-string "=key=" entry)) entry entries))))
        (when inheritance
          (parsebib-expand-xrefs entries inheritance))
        entries)
    (parsebib-error
     (save-excursion
       (goto-char (cadr err))
       (signal (car err) (list (concat (apply #'format (cddr err))
                                       (format " at position (%d,%d)" (line-number-at-pos) (current-column)))))))))

(defun parsebib-find-bibtex-dialect ()
  "Find the BibTeX dialect of a file if one is set.
This function looks for a local value of the variable
`bibtex-dialect' in the local variable block at the end of the
file.  Return nil if no dialect is found."
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search t)
          (bibtex-dialect-list (or (and (boundp 'bibtex-dialect-list)
                                        bibtex-dialect-list)
                                   '(BibTeX biblatex))))
      (when (re-search-backward (concat parsebib--bibtex-entry-start "comment") (- (point-max) 3000) t)
        (let ((comment (parsebib--@comment)))
          (when (and comment
                     (string-match-p "\\`{[ \n\t\r]*Local Variables:" comment)
                     (string-match-p "End:[ \n\t\r]*}\\'" comment)
                     (string-match (concat "bibtex-dialect: " (regexp-opt (mapcar #'symbol-name bibtex-dialect-list) t)) comment))
            (intern (match-string 1 comment))))))))

(cl-defun parsebib-parse-bib-buffer (&key entries strings expand-strings inheritance fields replace-TeX)
  "Parse the current buffer and return all BibTeX data.
Return a list of five elements: a hash table with the entries, a
hash table with the @String definitions, a list of @Preamble
definitions, a list of @Comments and the BibTeX dialect, if
present in the file.

If ENTRIES is a hash table with test function `equal', it is used
to store the entries.  Any existing entries with identical keys
are overwritten.  Similarly, if STRINGS is a hash table with test
function `equal', the @String definitions are stored in it.

If EXPAND-STRINGS is non-nil, abbreviations in the entries and
@String definitions are expanded using the @String definitions
already in STRINGS.  In addition, sequences of whitespace in
field values are collapsed into a single space and field values
are unquoted, i.e., the double quotes or braces around them are
removed.  Note that @String expansion, collapsing of whitespace
and prettifying TeX markup are not applied to fields listed in
`parsebib-postprocessing-excluded-fields', but unquoting is.

If INHERITANCE is non-nil, cross-references in the entries are
resolved: if the crossref field of an entry points to an entry
already in ENTRIES, the fields of the latter that do not occur in
the entry are added to it.  INHERITANCE indicates the inheritance
schema used for determining which fields inherit from which
fields.  It can be a symbol `BibTeX' or `biblatex', which means
to use the default inheritance schema for either dialect, or it
can be an explicit inheritance schema.  (See the variable
`parsebib--biblatex-inheritances' for details on the structure of
such an inheritance schema.)  It can also be the symbol t, in
which case the local variable block is checked for a
dialect (using the variable `bibtex-dialect'), or, if no such
local variable is found, the value of the variable
`bibtex-dialect'.

FIELDS is a list of the field names (as strings) to be read and
included in the result.  Fields not in the list are ignored,
except \"=key=\" and \"=type=\", which are always included.  Case
is ignored when comparing fields to the list in FIELDS.  If
FIELDS is nil, all fields are returned.

REPLACE-TEX indicates whether TeX markup should be replaced with
ASCII/Unicode characters.  See the variable
`parsebib-TeX-markup-replace-alist' for details."
  (or (and (hash-table-p entries)
           (eq (hash-table-test entries) 'equal))
      (setq entries (make-hash-table :test #'equal)))
  (or (and (hash-table-p strings)
           (eq (hash-table-test strings) 'equal))
      (setq strings (make-hash-table :test #'equal)))
  ;; Ensure  =key= and =type= are in `fields'.
  (if fields
      (setq fields (append (list "=key=" "=type=") fields)))
  (condition-case err
      (let ((dialect (or (parsebib-find-bibtex-dialect)
                         (and (boundp 'bibtex-dialect) bibtex-dialect)
                         'BibTeX))
            preambles comments)
        (save-excursion
          (goto-char (point-min))
          (cl-loop for item = (parsebib-find-next-item)
                   while item do
                   (cond
                    ((cl-equalp item "string") ; `cl-equalp' compares strings case-insensitively.
                     (let ((string (parsebib-read-string (if expand-strings strings))))
                       (if string
                           (puthash (car string) (cdr string) strings))))
                    ((cl-equalp item "preamble")
                     (push (parsebib--@preamble) preambles))
                    ((cl-equalp item "comment")
                     (push (parsebib--@comment) comments))
                    ((stringp item)
                     (let ((entry (parsebib-read-entry fields (if expand-strings strings) replace-TeX)))
                       (when entry
                         (puthash (cdr (assoc-string "=key=" entry)) entry entries))))))
          (when inheritance (parsebib-expand-xrefs entries (if (eq inheritance t) dialect inheritance)))
          (list entries strings (nreverse preambles) (nreverse comments) dialect)))
    (parsebib-error
     (save-excursion
       (goto-char (cadr err))
       (signal (car err) (list (concat (apply #'format (cddr err))
                                       (format " at position (%d,%d)" (line-number-at-pos) (current-column)))))))))

;;;; CSL-JSON API

(cl-defun parsebib-parse-json-buffer (&key entries stringify year-only fields)
  "Parse the current buffer and return all CSL-JSON data.
The return value is a hash table containing all the elements.
The hash table's keys are the \"id\" values of the entries, the
hash table's values are alists as returned by `json-parse-buffer'
or `json-read'

If ENTRIES is a hash table with test function `equal', it is used
to store the entries.  Any existing entries with identical keys
are overwritten.

If STRINGIFY is non-nil, JSON values that are not
strings (notably name and date fields) are converted to strings.
If additionally YEAR-ONLY is non-nil, dates are shortened to just
the year part.

FIELDS is a list of field names (as symbols) to be read and
included in the result.  Fields not in the list are ignored,
except `id' and `type', which are always included.  If FIELDS is
nil, all fields are returned.

If a JSON object is encountered that does not have an \"id\"
field, a `parsebib-error' is raised."
  (or (and (hash-table-p entries)
           (eq (hash-table-test entries) 'equal))
      (setq entries (make-hash-table :test #'equal)))
  (when fields
    (setq fields (append '(id type) fields)))
  (let ((parse (if (and (fboundp 'json-serialize)
                        (json-serialize '((test . 1)))) ; Returns nil if native json support isn't working for some reason.
                   (lambda ()
                     (json-parse-buffer :object-type 'alist))
                 (lambda ()
                   (let ((json-object-type 'alist))
                     (json-read))))))
    ;; We do not read the entire file in one go, but instead parse each entry
    ;; separately.  Large bibliographies would otherwise be returned as one
    ;; gigantic vector, which then needs to be converted to a hash table.  If we
    ;; need to convert some of the data because `stringify' is t, the data is
    ;; held in memory twice.
    (save-excursion
      (goto-char (point-min))
      ;; JSON is pretty strict, not even comments are allowed.  CSL-JSON
      ;; requires that the file is essentially one big array, so we know that
      ;; the first non-whitespace character in the file must be an opening
      ;; bracket;
      (if (not (looking-at-p "[\n\t ]*\\["))
          (error "[Parsebib Error] Not a valid CSL-JSON file"))
      (let ((continue t))
        (while continue
          ;; We also know that the first non-whitespace character after that
          ;; must be an opening brace:
          (skip-chars-forward "^{")
          (if-let* ((entry (funcall parse))
                    (id (alist-get 'id entry)))
              (progn
                (when fields
                  (setq entry (seq-filter (lambda (elt)
                                            (memq (car elt) fields))
                                          entry)))
                (puthash id (if stringify
                                (parsebib-stringify-json entry year-only)
                              entry)
                         entries))
            (signal 'parsebib-error (list (format "Malformed JSON entry at position (%d,%d)"
                                                  (line-number-at-pos) (current-column)))))
          ;; Parsing an entry moves point to the end of the entry.  The next
          ;; character must be a comma if there is another entry.  If we're not
          ;; seeing a comma, we've reached the end of the file:
          (if (not (looking-at-p "[\n\t ]*,"))
              (setq continue nil))))))
  entries)

(defun parsebib-stringify-json (entry &optional year-only)
  "Return ENTRY with all non-string values converted to strings.
ENTRY is a CSL-JSON entry in the form of an alist.  ENTRY is
modified in place.  Return value is ENTRY.  If YEAR-ONLY is
non-nil, date fields are shortened to just the year."
  (dolist (field entry)
    (unless (stringp (alist-get (car field) entry))
      (setf (alist-get (car field) entry)
            (parsebib-stringify-json-field (assq (car field) entry) year-only))))
  entry)

(defvar parsebib--json-name-fields  '(author
                                      collection-editor
                                      composer
                                      container-author
                                      director
                                      editor
                                      editorial-director
                                      illustrator
                                      interviewer
                                      original-author
                                      recipient
                                      reviewed-author
                                      translator))

(defvar parsebib--json-date-fields '(accessed
                                     container
                                     event-date
                                     issued
                                     original-date
                                     submitted))

(defvar parsebib--json-number-fields '(chapter-number
                                       collection-number
                                       edition
                                       issue
                                       number
                                       number-of-pages
                                       number-of-volumes
                                       volume))

(defvar parsebib-json-name-field-template "{non-dropping-particle }{family, }{given}{ dropping-particle}{, suffix}{literal}"
  "Template used to display name fields.")

(defvar parsebib-json-name-field-separator " and "
  "Separator used to concatenate names in a name field.")

(defvar parsebib-json-field-separator ", "
  "Separator used to concatenate items of array fields.")

(defun parsebib--process-template (template items)
  "Process TEMPLATE and return a formatted string.
ITEMS is an alist, the keys of which may occur in TEMPLATE.
Braced occurrences of the keys in ITEMS are replaced with the
corresponding values.  Note that the keys in ITEMS should be
symbols."
  (cl-flet ((create-replacements (match)
              (save-match-data
                (string-match "{\\([^A-Za-z]*\\)\\([A-Za-z][A-za-z-]+\\)\\([^A-Za-z]*\\)}" match)
                (let* ((pre (match-string 1 match))
                       (key (match-string 2 match))
                       (post (match-string 3 match))
                       (value (alist-get (intern key) items)))
                  (if value
                      (format "%s%s%s" pre value post)
                    "")))))
    (replace-regexp-in-string "{.*?}" #'create-replacements template nil t)))

(defun parsebib-stringify-json-field (field &optional short)
  "Return the value of FIELD as a string.
FIELD is a cons cell that constitutes a CSL-JSON field-value
pair.  The car is the key, the cdr the value.  If the value is a
string, return it with sequences of white space reduced to a
single space.  Otherwise, convert it into a string.  SHORT is
only relevant for date fields: if it is non-nil, return just a
year, or the string \"XXXX\" if no year part is present."
  (let ((key (car field))
        (value (cdr field)))
    (cond
     ((stringp value)
      (replace-regexp-in-string "[ \t\n\f[:space:]]+" " " value))

     ((numberp value)
      (format "%s" value))

     ((memq key parsebib--json-name-fields)
      (parsebib--json-stringify-name-field value))

     ((memq key parsebib--json-date-fields)
      (parsebib--json-stringify-date-field value short))

     ;; In CSL-JSON v1.0, the only array field besides name and date fields
     ;; is "categories".  It has an array of strings as value, so the `format'
     ;; isn't strictly necessary.  We do it this way just to be on the safe
     ;; side.
     ((arrayp value)
      (mapconcat (lambda (e) (format "%s" e)) value parsebib-json-field-separator))

     ;; This clause should never be reached.
     (t (replace-regexp-in-string "\n" " " (format "%s" value))))))

(defun parsebib--json-stringify-name-field (names)
  "Convert NAMES to a string.
NAMES is the value of a CSL-JSON name field, a vector of alists.
Conversion is done on the basis of
`parsebib-json-name-field-template': each field in this template
is replaced with the value of the field in NAME.  Fields that
have no value in NAME are ignored."
  (mapconcat (lambda (name)
               (parsebib--process-template parsebib-json-name-field-template name))
             names
             parsebib-json-name-field-separator))

(defun parsebib--json-stringify-date-field (date &optional short)
  "Convert DATE to a string.
DATE is the value of a CSL-JSON date field.  If SHORT is non-nil,
try to return only a year (in a date range, just the year of the
first date).  If no year part is present, SHORT returns
\"XXXX\"."
  (if short
      (if-let* ((date-parts (alist-get 'date-parts date))
                (first-date (aref date-parts 0))
                (year (aref first-date 0)))
          (format "%s" year)
        "XXXX")

    ;; Work with a copy of the original alist.
    (setq date (copy-sequence date))

    ;; Set start-date and end-date.
    (when-let* ((date-parts (alist-get 'date-parts date)))
      (let* ((start-date (aref date-parts 0))
             (end-date (if (= (length date-parts) 2)
                           (aref date-parts 1))))
        (setf (alist-get 'date-parts date nil :remove) nil)
        (setf (alist-get 'start-date date)
              (parsebib--json-stringify-date-part start-date))
        (if end-date (setf (alist-get 'end-date date)
                           (parsebib--json-stringify-date-part end-date)))))

    ;; Set season.
    (when-let* ((season (alist-get 'season date)))
      (if (numberp season)
          (setf (alist-get 'season date)
                (aref ["Spring" "Summer" "Autumn" "Winter"] (1- season)))))

    ;; Set circa.
    (when-let* ((circa (alist-get 'circa date)))
      (setf (alist-get 'circa date) "ca."))

    ;; Now convert the date.
    (parsebib--process-template "{circa }{season }{start-date}{/end-date}{literal}{raw}"
                                date)))

(defun parsebib--json-stringify-date-part (date-parts)
  "Convert DATE-PARTS into a string.
DATE-PARTS is a sequence with up to three numeric elements: a
year, a month and a day."
  (parsebib--process-template "{year}{-month}{-day}"
                              (seq-mapn #'cons '(year month day) date-parts)))

;;;; Format-independent API

(cl-defun parsebib-parse (files &key entries strings (display t) fields)
  "Parse one or more bibliography files.
FILES is the list of files to parse.  All bibliographic entries
in FILES are collected and returned in a single hash table.
FILES can be a list of `.bib' or `.json' files, or a combination
of these.  FILES can also be a string, which should be the path
to a single bibliography file.

ENTRIES, if provided, should be a hash table with test function
`equal', it is used to store the entries.  Any existing entries
with identical keys are overwritten.  If provided, ENTRIES is
also the return value.  If ENTRIES is nil, a new hash table is
created and returned.

STRINGS, similarly a hash table with test function `equal', is
used to store the @String definitions.  Although STRINGS is not
returned, it is modified in place and can therefore be used to
collect the @String definitions in the files being parsed.

If DISPLAY is non-nil, field values are returned in a way that is
suitable for display: in `.bib' files, @String abbreviations are
expanded, in `.json' files, values that are not strings are
converted to strings.  Furthermore, sequences of white space
characters (including newlines) are reduced to a single space.

Specifically, setting DISPLAY means setting the arguments
EXPAND-STRINGS and INHERITANCES in the function
`parsebib-parse-bib-buffer' and setting STRINGIFY and YEAR-ONLY
in the function `parsebib-parse-json-buffer'.  DISPLAY is simply
passed on to these arguments, which means that it can be set to
anything that INHERITANCES in `parsebib-parse-bib-buffer'
accepts.  (The other arguments only distinguish between nil and
non-nil.) Note that DISPLAY defaults to t.

FIELDS is a list of the field names to be read and included in
the result.  Fields not in the list are ignored.  Note that field
names should be strings; when parsing a `.json' file, they are
converted to symbols.  See the doc strings of
`parsebib-parse-bib-buffer' and `parsebib-parse-json-buffer' for
details.  If FIELDS is nil, all fields are returned."
  (or (and (hash-table-p entries)
           (eq (hash-table-test entries) 'equal))
      (setq entries (make-hash-table :test #'equal)))
  (or (and (hash-table-p strings)
           (eq (hash-table-test strings) 'equal))
      (setq strings (make-hash-table :test #'equal)))
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (cond
       ((string= (file-name-extension file t) ".bib")
        (parsebib-parse-bib-buffer :entries entries
                                   :strings strings
                                   :expand-strings display
                                   :inheritance display
                                   :fields fields
                                   :replace-TeX display))
       ((string= (file-name-extension file t) ".json")
        (parsebib-parse-json-buffer :entries entries
                                    :stringify display
                                    :year-only display
                                    :fields (mapcar #'intern fields)))
       (t (error "[Parsebib] Not a bibliography file: %s" file)))))
  entries)

(provide 'parsebib)

;;; parsebib.el ends here
