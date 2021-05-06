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

;;; parsebib-test.el ends here
