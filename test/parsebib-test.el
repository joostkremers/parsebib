;;; parsebib-test.el --- Tests for parsebib

(require 'parsebib)

;; Note: tests are named with the prefix `parsebib-test-' followed by the name
;; of the function being tested, without the `parsebib-' or `parsebib--' prefix.

(ert-deftest parsebib-test-json-stringify-date-part ()
  (should (string= (parsebib--json-stringify-date-part [2021 22 4]) "2021-22-4"))
  (should (string= (parsebib--json-stringify-date-part [2021 22]) "2021-22"))
  (should (string= (parsebib--json-stringify-date-part [2021]) "2021")))

(ert-deftest parsebib-test-json-stringify-date-field ()
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004]])))
                   "2004"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004 4 22]])))
                   "2004-4-22"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004 4 22] [2021 4 22]])))
                   "2004-4-22/2021-4-22"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004] [2021]])))
                   "2004/2021"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004 4]])
                                                          (circa . t)))
                   "ca. 2004-4"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (date-parts . [[2004]])
                                                          (season . 1)))
                   "Spring 2004"))
  (should (string= (parsebib--json-stringify-date-field '(issued
                                                          (literal . "April 2004")))
                   "April 2004")))

(ert-deftest parsebib-test-json-stringify-name-field ()
  (should (string= (parsebib--json-stringify-name-field [((family . "Anderson")
                                                          (given . "John R."))
                                                         ((family . "Bothell")
                                                          (given . "Daniel"))])
                   "Anderson, John R. and Bothell, Daniel"))
  (should (string= (parsebib--json-stringify-name-field [((family . "Koning")
                                                          (given . "Willem")
                                                          (non-dropping-particle . "de"))
                                                         ((family . "Beethoven")
                                                          (dropping-particle . "van")
                                                          (given . "Ludwig"))])
                   "de Koning, Willem and Beethoven, Ludwig van"))
  (should (string= (parsebib--json-stringify-name-field [((family . "Gates")
                                                          (given . "Bill")
                                                          (suffix . "III"))])
                   "Gates, Bill, III"))
  (should (string= (parsebib--json-stringify-name-field [((literal . "Michigan Institute of Technology"))])
                   "Michigan Institute of Technology")))

;; Test `parsebib-stringify-json-field' with name fields.
(ert-deftest parsebib-test-stringify-json-field--name-fields ()
  (should (string= (parsebib-stringify-json-field '(author . [((family . "Anderson")
                                                               (given . "John R."))
                                                              ((family . "Bothell")
                                                               (given . "Daniel"))]))
                   "Anderson, John R. and Bothell, Daniel"))
  (should (string= (parsebib-stringify-json-field '(editor . [((family . "Koning")
                                                               (given . "Willem")
                                                               (non-dropping-particle . "de"))
                                                              ((family . "Beethoven")
                                                               (dropping-particle . "van")
                                                               (given . "Ludwig"))]))
                   "de Koning, Willem and Beethoven, Ludwig van"))
  (should (string= (parsebib-stringify-json-field '(translator . [((family . "Gates")
                                                                   (given . "Bill")
                                                                   (suffix . "III"))]))
                   "Gates, Bill, III"))
  (should (string= (parsebib-stringify-json-field '(recipient . [((literal . "Michigan Institute of Technology"))]))
                   "Michigan Institute of Technology")))


;; Test `parsebib-stringify-json-field' with date fields.
(ert-deftest parsebib-test-stringify-json-field--date-fields ()
  (should (string= (parsebib-stringify-json-field '(container
                                                    (date-parts . [[2004 4 22] [2021 4 22]])))
                   "2004-4-22/2021-4-22"))
  (should (string= (parsebib-stringify-json-field '(submitted
                                                    (date-parts . [[2004]])))
                   "2004"))
  (should (string= (parsebib-stringify-json-field '(issued
                                                    (date-parts . [[2004 4]])
                                                    (circa . t)))
                   "ca. 2004-4"))
  (should (string= (parsebib-stringify-json-field '(event-date
                                                    (date-parts . [[2004]])
                                                    (season . 1)))
                   "Spring 2004"))
  (should (string= (parsebib-stringify-json-field '(accessed
                                                    (literal . "April 2004")))
                   "April 2004")))

;; Test `parsebib-stringify-json-field' with string fields.
(ert-deftest parsebib-test-stringify-json-field--string-fields ()
  (should (string= (parsebib-stringify-json-field '(title . "The Minimalist Program"))
                   "The Minimalist Program"))
  (should (string= (parsebib-stringify-json-field '(ISBN . "1-01-XXXXXX-X"))
                   "1-01-XXXXXX-X")))

;; Test `parsebib-stringify-json-field' with number fields.
(ert-deftest parsebib-test-stringify-json-field--number-fields ()
  (should (string= (parsebib-stringify-json-field '(volume . 3))
                   "3"))
  (should (string= (parsebib-stringify-json-field '(page . 155))
                   "155")))

;; Test `parsebib-stringify-json-field' with array fields.
(ert-deftest parsebib-test-stringify-json-field--array-fields ()
  (should (string= (parsebib-stringify-json-field '(categories . ["fiction" "horror"]))
                   "fiction, horror")))

;;; Tests for `parsebib-clean-TeX-markup'

(ert-deftest parsebib-clean-TeX-markup-dashes ()
  (should (equal (parsebib-clean-TeX-markup "---") "—"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash") "—"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash and") "—and"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash  and") "—and"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash{}") "—"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash{}and") "—and"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash{} and") "— and"))
  (should (equal (parsebib-clean-TeX-markup "\\textemdash{}  and") "— and"))

  (should (equal (parsebib-clean-TeX-markup "--") "–"))
  (should (equal (parsebib-clean-TeX-markup "\\textendash") "–"))
  (should (equal (parsebib-clean-TeX-markup "\\textendash{}") "–")))

(ert-deftest parsebib-clean-TeX-markup-math-and-text-mode-commands ()
  (should (equal (parsebib-clean-TeX-markup "\\ddag{} \\textdaggerdbl") "‡ ‡"))
  (should (equal (parsebib-clean-TeX-markup "10\\textpertenthousand") "10‱"))
  (should (equal (parsebib-clean-TeX-markup "200\\textperthousand.") "200‰."))
  (should (equal (parsebib-clean-TeX-markup "\\textquestiondown") "¿"))
  (should (equal (parsebib-clean-TeX-markup "\\P 3.2") "¶3.2"))
  (should (equal (parsebib-clean-TeX-markup "\\textdollar") "$"))
  (should (equal (parsebib-clean-TeX-markup "\\S 5.2") "§5.2"))
  (should (equal (parsebib-clean-TeX-markup "\\ldots{} [\\dots] \\textellipsis and")
                 "… […] …and")))

(ert-deftest parsebib-clean-TeX-markup-nonletter-diacritics-without-braces ()
  ;; No space is needed after a nonletter diacritic commands.
  (should (equal (parsebib-clean-TeX-markup "\\\"a") "a\N{COMBINING DIAERESIS}"))
  (should (equal (parsebib-clean-TeX-markup "\\'a")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\.a")  "a\N{COMBINING DOT ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\=a")  "a\N{COMBINING MACRON}"))
  (should (equal (parsebib-clean-TeX-markup "\\^a")  "a\N{COMBINING CIRCUMFLEX ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\`a")  "a\N{COMBINING GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\~a")  "a\N{COMBINING TILDE}"))
  (should (equal (parsebib-clean-TeX-markup "\\|a")  "a\N{COMBINING COMMA ABOVE}"))
  ;; Spaces are possible, though:
  (should (equal (parsebib-clean-TeX-markup "\\' a")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\'  a")  "a\N{COMBINING ACUTE ACCENT}")))

(ert-deftest parsebib-clean-TeX-markup-letter-diacritics-without-braces ()
  ;; Diacritic commands that consist of a single letter require a space.
  (should (equal (parsebib-clean-TeX-markup "\\b a") "a\N{COMBINING MACRON BELOW}"))
  (should (equal (parsebib-clean-TeX-markup "\\c c") "c\N{COMBINING CEDILLA}"))
  (should (equal (parsebib-clean-TeX-markup "\\d a") "a\N{COMBINING DOT BELOW}"))
  (should (equal (parsebib-clean-TeX-markup "\\H a") "a\N{COMBINING DOUBLE ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\k a") "a\N{COMBINING OGONEK}"))
  (should (equal (parsebib-clean-TeX-markup "\\U a") "a\N{COMBINING DOUBLE VERTICAL LINE ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\u a") "a\N{COMBINING BREVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\v a") "a\N{COMBINING CARON}"))
  (should (equal (parsebib-clean-TeX-markup "\\f a") "a\N{COMBINING INVERTED BREVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\G a") "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\h a") "a\N{COMBINING HOOK ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\C a") "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\r a") "a\N{COMBINING RING ABOVE}"))
  ;; More than one space should also work:
  (should (equal (parsebib-clean-TeX-markup "\\b  a") "a\N{COMBINING MACRON BELOW}"))
  (should (equal (parsebib-clean-TeX-markup "\\b   a") "a\N{COMBINING MACRON BELOW}"))
  ;; It shouldn't work without space. Since something like "\ba after" is
  ;; essentially a command without an (explicit) argument, it should remain
  ;; unchanged.
  (should (equal (parsebib-clean-TeX-markup "before \\ba after") "before \\ba after")))

(ert-deftest parsebib-clean-TeX-markup-diacritics-with-braces ()
  ;; Diacritic commands may use braces to mark the argument.
  (should (equal (parsebib-clean-TeX-markup "\\\"{a}") "a\N{COMBINING DIAERESIS}"))
  (should (equal (parsebib-clean-TeX-markup "\\'{a}")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\.{a}")  "a\N{COMBINING DOT ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\={a}")  "a\N{COMBINING MACRON}"))
  (should (equal (parsebib-clean-TeX-markup "\\^{a}")  "a\N{COMBINING CIRCUMFLEX ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\`{a}")  "a\N{COMBINING GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\b{a}")  "a\N{COMBINING MACRON BELOW}"))
  (should (equal (parsebib-clean-TeX-markup "\\c{c}")  "c\N{COMBINING CEDILLA}"))
  (should (equal (parsebib-clean-TeX-markup "\\d{a}")  "a\N{COMBINING DOT BELOW}"))
  (should (equal (parsebib-clean-TeX-markup "\\H{a}")  "a\N{COMBINING DOUBLE ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\k{a}")  "a\N{COMBINING OGONEK}"))
  (should (equal (parsebib-clean-TeX-markup "\\U{a}")  "a\N{COMBINING DOUBLE VERTICAL LINE ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\u{a}")  "a\N{COMBINING BREVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\v{a}")  "a\N{COMBINING CARON}"))
  (should (equal (parsebib-clean-TeX-markup "\\~{a}")  "a\N{COMBINING TILDE}"))
  (should (equal (parsebib-clean-TeX-markup "\\|{a}")  "a\N{COMBINING COMMA ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\f{a}")  "a\N{COMBINING INVERTED BREVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\G{a}")  "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\h{a}")  "a\N{COMBINING HOOK ABOVE}"))
  (should (equal (parsebib-clean-TeX-markup "\\C{a}")  "a\N{COMBINING DOUBLE GRAVE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\r{a}")  "a\N{COMBINING RING ABOVE}"))
  ;; There may be spaces between the command and the argument.
  (should (equal (parsebib-clean-TeX-markup "\\' {a}")  "a\N{COMBINING ACUTE ACCENT}"))
  (should (equal (parsebib-clean-TeX-markup "\\'  {a}")  "a\N{COMBINING ACUTE ACCENT}")))

(ert-deftest parsebib-clean-TeX-markup-escapable-characters ()
  (should (equal (parsebib-clean-TeX-markup "percent: \\%")
                 "percent: %"))
  (should (equal (parsebib-clean-TeX-markup "ampersand: \\&")
                 "ampersand: &"))
  (should (equal (parsebib-clean-TeX-markup "hash: \\#")
                 "hash: #"))
  (should (equal (parsebib-clean-TeX-markup "dollar: \\$")
                 "dollar: $")))

(ert-deftest parsebib-clean-TeX-markup-quotes ()
  (should (equal (parsebib-clean-TeX-markup "``double'' quotes") "\N{LEFT DOUBLE QUOTATION MARK}double\N{RIGHT DOUBLE QUOTATION MARK} quotes"))
  (should (equal (parsebib-clean-TeX-markup "`single' quotes") "\N{LEFT SINGLE QUOTATION MARK}single\N{RIGHT SINGLE QUOTATION MARK} quotes")))

(ert-deftest parsebib-clean-TeX-markup-textit ()
  (should (equal-including-properties
           (parsebib-clean-TeX-markup "The verb \\textit{krijgen} as an undative verb.")
           #("The verb krijgen as an undative verb." 9 16
             (face italic))))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'markdown))
             (parsebib-clean-TeX-markup "The verb \\textit{krijgen} as an undative verb."))
           "The verb *krijgen* as an undative verb."))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'org))
             (parsebib-clean-TeX-markup "The verb \\textit{krijgen} as an undative verb."))
           "The verb /krijgen/ as an undative verb.")))

(ert-deftest parsebib-clean-TeX-markup-emph ()
  (should (equal-including-properties
           (parsebib-clean-TeX-markup "The verb \\emph{krijgen} as an undative verb.")
           #("The verb krijgen as an undative verb." 9 16
             (face italic))))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'markdown))
             (parsebib-clean-TeX-markup "The verb \\emph{krijgen} as an undative verb."))
           "The verb *krijgen* as an undative verb."))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'org))
             (parsebib-clean-TeX-markup "The verb \\emph{krijgen} as an undative verb."))
           "The verb /krijgen/ as an undative verb.")))

(ert-deftest parsebib-clean-TeX-markup-textbf ()
  (should (equal-including-properties
           (parsebib-clean-TeX-markup "The verb \\textbf{krijgen} as an undative verb.")
           #("The verb krijgen as an undative verb." 9 16
             (face bold))))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'markdown))
             (parsebib-clean-TeX-markup "The verb \\textbf{krijgen} as an undative verb."))
           "The verb **krijgen** as an undative verb."))
  (should (equal
           (let ((parsebib-TeX-cleanup-target 'org))
             (parsebib-clean-TeX-markup "The verb \\textbf{krijgen} as an undative verb."))
           "The verb *krijgen* as an undative verb.")))

(ert-deftest parsebib-clean-TeX-markup-textsc ()
  (should (equal
           (parsebib-clean-TeX-markup "The verb \\textsc{krijgen} as an undative verb.")
           "The verb KRIJGEN as an undative verb.")))

(ert-deftest parsebib-clean-TeX-markup-nested-macros ()
  (should (equal (parsebib-clean-TeX-markup "\\textit{\\foo{bar}}")
                 #("bar" 0 3 (face italic))))
  (should (equal (parsebib-clean-TeX-markup "\\textit{\\foo}}")
                 #("\\foo" 0 4 (face italic)))))

(ert-deftest parsebib-clean-TeX-markup-nonascii-letters-with-braces ()
  ;; The braces should be removed and the space after it retained.
  (should (equal (parsebib-clean-TeX-markup "\\AA{} and") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\AE{} and") "\N{LATIN CAPITAL LETTER AE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\DH{} and") "\N{LATIN CAPITAL LETTER ETH} and"))
  (should (equal (parsebib-clean-TeX-markup "\\DJ{} and") "\N{LATIN CAPITAL LETTER ETH} and"))
  (should (equal (parsebib-clean-TeX-markup "\\L{} and")  "\N{LATIN CAPITAL LETTER L WITH STROKE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\SS{} and") "\N{LATIN CAPITAL LETTER SHARP S} and"))
  (should (equal (parsebib-clean-TeX-markup "\\NG{} and") "\N{LATIN CAPITAL LETTER ENG} and"))
  (should (equal (parsebib-clean-TeX-markup "\\OE{} and") "\N{LATIN CAPITAL LIGATURE OE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\O{} and")  "\N{LATIN CAPITAL LETTER O WITH STROKE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\TH{} and") "\N{LATIN CAPITAL LETTER THORN} and"))
  (should (equal (parsebib-clean-TeX-markup "\\aa{} and") "\N{LATIN SMALL LETTER A WITH RING ABOVE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\ae{} and") "\N{LATIN SMALL LETTER AE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\dh{} and") "\N{LATIN SMALL LETTER ETH} and"))
  (should (equal (parsebib-clean-TeX-markup "\\dj{} and") "\N{LATIN SMALL LETTER ETH} and"))
  (should (equal (parsebib-clean-TeX-markup "\\l{} and")  "\N{LATIN SMALL LETTER L WITH STROKE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\ss{} and") "\N{LATIN SMALL LETTER SHARP S} and"))
  (should (equal (parsebib-clean-TeX-markup "\\ng{} and") "\N{LATIN SMALL LETTER ENG} and"))
  (should (equal (parsebib-clean-TeX-markup "\\oe{} and") "\N{LATIN SMALL LIGATURE OE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\o{} and")  "\N{LATIN SMALL LETTER O WITH STROKE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\th{} and") "\N{LATIN SMALL LETTER THORN} and"))
  (should (equal (parsebib-clean-TeX-markup "\\ij{} and") "ij and"))
  (should (equal (parsebib-clean-TeX-markup "\\i{} and")  "\N{LATIN SMALL LETTER DOTLESS I} and"))
  (should (equal (parsebib-clean-TeX-markup "\\j{} and")  "\N{LATIN SMALL LETTER DOTLESS J} and"))
  ;; More than one space should work as well.
  (should (equal (parsebib-clean-TeX-markup "\\AA{}  and")  "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and"))
  (should (equal (parsebib-clean-TeX-markup "\\AA{}   and") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE} and")))

(ert-deftest parsebib-clean-TeX-markup-nonascii-letters-without-braces ()
  ;; The space should be removed.
  (should (equal (parsebib-clean-TeX-markup "\\AA n") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\AE n") "\N{LATIN CAPITAL LETTER AE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\DH n") "\N{LATIN CAPITAL LETTER ETH}n"))
  (should (equal (parsebib-clean-TeX-markup "\\DJ n") "\N{LATIN CAPITAL LETTER ETH}n"))
  (should (equal (parsebib-clean-TeX-markup "\\L n")  "\N{LATIN CAPITAL LETTER L WITH STROKE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\SS n") "\N{LATIN CAPITAL LETTER SHARP S}n"))
  (should (equal (parsebib-clean-TeX-markup "\\NG n") "\N{LATIN CAPITAL LETTER ENG}n"))
  (should (equal (parsebib-clean-TeX-markup "\\OE n") "\N{LATIN CAPITAL LIGATURE OE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\O n")  "\N{LATIN CAPITAL LETTER O WITH STROKE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\TH n") "\N{LATIN CAPITAL LETTER THORN}n"))
  (should (equal (parsebib-clean-TeX-markup "\\aa n") "\N{LATIN SMALL LETTER A WITH RING ABOVE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\ae n") "\N{LATIN SMALL LETTER AE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\dh n") "\N{LATIN SMALL LETTER ETH}n"))
  (should (equal (parsebib-clean-TeX-markup "\\dj n") "\N{LATIN SMALL LETTER ETH}n"))
  (should (equal (parsebib-clean-TeX-markup "\\l n")  "\N{LATIN SMALL LETTER L WITH STROKE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\ss n") "\N{LATIN SMALL LETTER SHARP S}n"))
  (should (equal (parsebib-clean-TeX-markup "\\ng n") "\N{LATIN SMALL LETTER ENG}n"))
  (should (equal (parsebib-clean-TeX-markup "\\oe n") "\N{LATIN SMALL LIGATURE OE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\o n")  "\N{LATIN SMALL LETTER O WITH STROKE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\th n") "\N{LATIN SMALL LETTER THORN}n"))
  (should (equal (parsebib-clean-TeX-markup "\\ij n") "ijn"))
  (should (equal (parsebib-clean-TeX-markup "\\i n")  "\N{LATIN SMALL LETTER DOTLESS I}n"))
  (should (equal (parsebib-clean-TeX-markup "\\j n")  "\N{LATIN SMALL LETTER DOTLESS J}n"))
  ;; More than one space should work as well.
  (should (equal (parsebib-clean-TeX-markup "\\AA  n")  "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  (should (equal (parsebib-clean-TeX-markup "\\AA   n") "\N{LATIN CAPITAL LETTER A WITH RING ABOVE}n"))
  ;; If there is no space, treat it as an unknown command.
  (should (equal (parsebib-clean-TeX-markup "\\AAn")  "\\AAn")))

(ert-deftest parsebib-clean-TeX-markup-other-commands ()
  ;; Do not change commands with no arguments.
  (should (equal (parsebib-clean-TeX-markup "\\LaTeX and") "\\LaTeX and"))
  ;; Commands with an empty set of braces should remain, the braces should be removed.
  (should (equal (parsebib-clean-TeX-markup "\\LaTeX{} and") "\\LaTeX and"))
  ;; Obligatory arguments should replace the command.
  (should (equal (parsebib-clean-TeX-markup "\\foo{bar} and") "bar and"))
  ;; Optional arguments should be removed, even empty ones.
  (should (equal (parsebib-clean-TeX-markup "\\foo[]{bar} and") "bar and"))
  (should (equal (parsebib-clean-TeX-markup "\\foo[bar]{baz} and") "baz and"))
  (should (equal (parsebib-clean-TeX-markup "\\foo[bar][baz]{boo} and") "boo and"))
  (should (equal (parsebib-clean-TeX-markup "\\foo[bar][baz]{} and") "\\foo and")))

(ert-deftest parsebib-clean-TeX-markup-braces ()
  ;; Braces not part of a command should be removed.
  (should (equal (parsebib-clean-TeX-markup "The {UN} should be all-caps.") "The UN should be all-caps.")))

;;; Test for reading the .bib file for display.

;; Test if @String abbreviations are expanded.
(ert-deftest parsebib-test-parse-bib-buffer-@Strings ()
  (should (equal
           (with-temp-buffer
             (insert "@String{MGrt = {Berlin: Mouton de Gruyter}}\n"
                     "\n"
                     "@book{Alexiadou:Haegeman:Stavrou2007,\n"
                     "	year = {2007},\n"
                     "	publisher = MGrt,\n"
                     "	title = {Noun Phrase in the Generative Perspective},\n"
                     "	author = {Alexiadou, Artemis and Haegeman, Liliane and Stavrou, Melita},\n"
                     "	timestamp = {2013-09-25 12:00:00 (CET)},\n"
                     "	file = {a/Alexiadou_Haegeman_Stavrou2007.pdf}}\n")
             (let ((results (parsebib-parse-bib-buffer :expand-strings t)))
               (alist-get "publisher" (gethash "Alexiadou:Haegeman:Stavrou2007" (car results))
                          nil nil #'equal)))
           "Berlin: Mouton de Gruyter")))

;; Test if braces around the `file' field are removed.
(ert-deftest parsebib-test-parse-bib-buffer-braces-in-file-field ()
  (should (equal
           (with-temp-buffer
             (insert "@String{MGrt = {Berlin: Mouton de Gruyter}}\n"
                     "\n"
                     "@book{Alexiadou:Haegeman:Stavrou2007,\n"
                     "	year = {2007},\n"
                     "	publisher = MGrt,\n"
                     "	title = {Noun Phrase in the Generative Perspective},\n"
                     "	author = {Alexiadou, Artemis and Haegeman, Liliane and Stavrou, Melita},\n"
                     "	timestamp = {2013-09-25 12:00:00 (CET)},\n"
                     "	file = {a/Alexiadou_Haegeman_Stavrou2007.pdf}}\n")
             (let ((results (parsebib-parse-bib-buffer :expand-strings t)))
               (alist-get "file" (gethash "Alexiadou:Haegeman:Stavrou2007" (car results))
                          nil nil #'equal)))
           "a/Alexiadou_Haegeman_Stavrou2007.pdf")))

;; Test if TeX markup is handled.
(ert-deftest parsebib-test-parse-bib-buffer-TeX-markup ()
  (should (equal
           (with-temp-buffer
             (insert "@Article{Broekhuis:Cornips2012,\n"
                     "	doi = {10.1515/ling-2012-0039},\n"
                     "	file = {b/Broekhuis_Cornips2012.pdf},\n"
                     "	pages = {1205-1249},\n"
                     "	volume = {50},\n"
                     "	number = {6},\n"
                     "	date = {2012},\n"
                     "	journaltitle = {Linguistics},\n"
                     "	title = {The Verb \\textit{krijgen} `to get' as an Undative Verb},\n"
                     "	author = {Broekhuis, Hans and Cornips, Leonie},\n"
                     "	timestamp = {2019-01-16 23:38:31 (CET)}}\n")
             (let ((results (parsebib-parse-bib-buffer :replace-TeX t)))
               (alist-get "title" (gethash "Broekhuis:Cornips2012" (car results))
                          nil nil #'equal)))
           #("The Verb krijgen ‘to get’ as an Undative Verb" 9 16 (face italic)))))

;; Test if white space is collapsed.
(ert-deftest parsebib-test-parse-bib-buffer-collapse-whitespace ()
  (should (equal
           (with-temp-buffer
             (insert "@String{MGrt = {Berlin: Mouton de Gruyter}}\n"
                     "\n"
                     "@book{Alexiadou:Haegeman:Stavrou2007,\n"
                     "	year = {2007},\n"
                     "	publisher = MGrt,\n"
                     "	title = {Noun Phrase  in the \n  Generative Perspective},\n"
                     "	author = {Alexiadou, Artemis and Haegeman, Liliane and Stavrou, Melita},\n"
                     "	timestamp = {2013-09-25 12:00:00 (CET)},\n"
                     "	file = {a/Alexiadou_Haegeman_Stavrou2007.pdf}}\n")
             (let ((results (parsebib-parse-bib-buffer :expand-strings t)))
               (alist-get "title" (gethash "Alexiadou:Haegeman:Stavrou2007" (car results))
                          nil nil #'equal)))
           "Noun Phrase in the Generative Perspective")))

;; Test if sequences of spaces in file names are retained, even if @strings are expanded.
(ert-deftest parsebib-test-parse-bib-buffer-dont-collapse-whitespace-in-file-field ()
  (should (equal
           (with-temp-buffer
             (insert
              "@inproceedings{ahnIdentifyingCPUBottlenecks,\n"
              "  title = {Identifying {{On-}}/{{Off-CPU Bottlenecks Together}} with {{Blocked Samples}} {\textbar} {{USENIX}}},\n"
              "  shorttitle = {{{BCOZ}}},\n"
              "  author = {Ahn, Minwoo and Han, Jeongmin and Kwon, Youngjin and Jeong, Jinkyu},\n"
              "  urldate = {2024-10-09},\n"
              "  langid = {english},\n"
              "  file = {/Users/xxxx/Zotero/storage/ZJVUZD8F/Ahn et al. - Identifying On-Off-CPU Bottlenecks Together with Blocked Samples  USENIX.pdf}}\n")
             (let ((results (parsebib-parse-bib-buffer :expand-strings t)))
               (alist-get "file" (gethash "ahnIdentifyingCPUBottlenecks" (car results))
                          nil nil #'equal)))
           "/Users/xxxx/Zotero/storage/ZJVUZD8F/Ahn et al. - Identifying On-Off-CPU Bottlenecks Together with Blocked Samples  USENIX.pdf")))

;;; Test the RDP
(ert-deftest parsebib-test-@comment ()
  (should (equal
           (with-temp-buffer
             (insert "@Comment{ -*-coding: utf-8 -*- }\n")
             (goto-char (point-min))
             (parsebib--@comment))
           "{ -*-coding: utf-8 -*- }"))
  (should (equal
           (with-temp-buffer
             (insert "@Comment -*-coding: utf-8 -*-\n")
             (goto-char (point-min))
             (parsebib--@comment))
           "-*-coding: utf-8 -*-"))
  (should (equal
           (with-temp-buffer
             (insert "@Comment{\n"
                     "    Local Variables:\n"
                     "    bibtex-dialect: biblatex\n"
                     "    End:\n"
                     "}\n")
             (goto-char (point-min))
             (parsebib--@comment))
           (concat "{\n"
                   "    Local Variables:\n"
                   "    bibtex-dialect: biblatex\n"
                   "    End:\n"
                   "}"))))

(ert-deftest parsebib-test-@string ()
  ;; @String definition with curly braces.
  (should (equal
           (with-temp-buffer
             (insert "@String{MGrt = {Berlin: Mouton de Gruyter}}")
             (goto-char (point-min))
             (parsebib--@string))
           (list "MGrt" "{Berlin: Mouton de Gruyter}")))
  ;; @String definition with double quotes.
  (should (equal
           (with-temp-buffer
             (insert "@String{LI = \"Linguistic Inquiry\"}")
             (goto-char (point-min))
             (parsebib--@string))
           (list "LI" "\"Linguistic Inquiry\"")))
  ;; @String definition with @String abbrev.
  (should (equal
           (with-temp-buffer
             (insert "@String{CUP = {Cambridge: Cambridge } # UP}")
             (goto-char (point-min))
             (parsebib--@string))
           (list "CUP" "{Cambridge: Cambridge }" "UP")))
  ;; @String definition missing closing curly brace.
  (should-error (with-temp-buffer
                  (insert "@String{CUP = {Cambridge: Cambridge } # UP\n")
                  (goto-char (point-min))
                  (parsebib--@string))
                :type 'parsebib-error)
  ;; @String abbreviation without expansion.
  (should (equal (with-temp-buffer
                   (insert "@Article{Potapov_2016aa,\n"
                           "    author = {Potapov, Denis and Sukochev, Fedor and Zanin, Dmitriy},\n"
                           "    month = dec,\n"
                           "    title = {{Krein's trace theorem revisited}},\n"
                           "    url = {http://arxiv.org/abs/1701.00697v1},\n"
                           "    year = {2016}\n"
                           "}\n")
                   (goto-char (point-min))
                   (let ((results (parsebib-read-entry nil #s(hash-table size 10 data nil test equal))))
                     (alist-get "month" results nil nil #'equal)))
                 "dec")))

(ert-deftest parsebib-test-read-entry-nested-braces ()
  (should (equal
           (with-temp-buffer
             (insert "@article{10.1162/coli_a_00528,\n"
                     "    title = {Usage-based {Grammar Induction} from {Minimal Cognitive Principles}}\n"
                     "}\n")
             (goto-char (point-min))
             (let ((results (parsebib-read-entry)))
               (alist-get "title" results nil nil #'equal)))
           "{Usage-based {Grammar Induction} from {Minimal Cognitive Principles}}")))

(ert-deftest parsebib-test-read-entry-after-last-field ()
  ;; The last field in an entry does not have to have a comma after it:
  (should (equal
           (with-temp-buffer
             (insert "@article{10.1162/coli_a_00528,\n"
                     "    title = {Usage-based Grammar Induction from Minimal Cognitive Principles}\n"
                     "}\n")
             (goto-char (point-min))
             (let ((results (parsebib-read-entry)))
               (cons (alist-get "=key=" results nil nil #'equal)
                     (alist-get "title" results nil nil #'equal))))
           (cons "10.1162/coli_a_00528"
                 "{Usage-based Grammar Induction from Minimal Cognitive Principles}")))
  ;; But there *may* be a comma after the last field:
  (should (equal
           (with-temp-buffer
             (insert "@article{10.1162/coli_a_00528,\n"
                     "    title = {Usage-based Grammar Induction from Minimal Cognitive Principles},\n"
                     "}\n")
             (goto-char (point-min))
             (let ((results (parsebib-read-entry)))
               (cons (alist-get "=key=" results nil nil #'equal)
                     (alist-get "title" results nil nil #'equal))))
           (cons "10.1162/coli_a_00528"
                 "{Usage-based Grammar Induction from Minimal Cognitive Principles}"))))

;; Test braces and parentheses around an entry:
(ert-deftest parsebib-test-read-entry-parentheses ()
  (should (equal
           (with-temp-buffer
             (insert "@book{Alexiadou:Haegeman:Stavrou2007,\n"
                     "	year = {2007},\n"
                     "	publisher = MGrt,\n"
                     "	title = {Noun Phrase in the Generative Perspective},\n"
                     "	author = {Alexiadou, Artemis and Haegeman, Liliane and Stavrou, Melita},\n"
                     "	timestamp = {2013-09-25 12:00:00 (CET)},\n"
                     "	file = {a/Alexiadou_Haegeman_Stavrou2007.pdf}}\n")
             (goto-char (point-min))
             (let ((results (parsebib-read-entry)))
               (alist-get "=key=" results nil nil #'equal)))
           "Alexiadou:Haegeman:Stavrou2007"))
  (should (equal
           (with-temp-buffer
             (insert "@book(Alexiadou:Haegeman:Stavrou2007,\n"
                     "	year = {2007},\n"
                     "	publisher = MGrt,\n"
                     "	title = {Noun Phrase in the Generative Perspective},\n"
                     "	author = {Alexiadou, Artemis and Haegeman, Liliane and Stavrou, Melita},\n"
                     "	timestamp = {2013-09-25 12:00:00 (CET)},\n"
                     "	file = {a/Alexiadou_Haegeman_Stavrou2007.pdf})\n")
             (goto-char (point-min))
             (let ((results (parsebib-read-entry)))
               (alist-get "=key=" results nil nil #'equal)))
           "Alexiadou:Haegeman:Stavrou2007")))

;;; parsebib-test.el ends here
