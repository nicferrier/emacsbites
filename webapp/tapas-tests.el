;;; tests for tapas.el

(ert-deftest tapas-creole->bootstrap ()
  (should
   (equal
    (tapas-creole->bootstrap
     '((h1 . "text")
       (hr)
       (h2 . "more text")
       (hr)))
    ;; Here's what it should equal
    '((plugin-html . "<div>")
      (h1 . "text")
      (hr)
      (plugin-html . "</div><div class=\"section\">")
      (h2 . "more text")
      (hr)
      (plugin-html . "</div><div class=\"section\">")
      (plugin-html . "</div>")))))


;;; tapas-tests.el ends here
