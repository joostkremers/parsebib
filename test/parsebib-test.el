;;; parsebib-test.el --- Tests for parsebib

(require 'parsebib)

;; Note: tests are named with the prefix `parsebib-test-' followed by the name
;; of the function being tested, without the `parsebib-' or `parsebib--' prefix.

(ert-deftest parsebib-test-json-stringify-date-part ()
  (should (string= (parsebib--json-stringify-date-part [2021 22 4]) "2021-22-4"))
  (should (string= (parsebib--json-stringify-date-part [2021 22]) "2021-22"))
  (should (string= (parsebib--json-stringify-date-part [2021]) "2021")))

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

;;; parsebib-test.el ends here
