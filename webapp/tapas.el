;;; tapas.el -- a webapp presenting the emacslisp tapas

(require 'elnode)

(elnode-app tapas-root creole cl)

(defconst tapas-docroot
  (file-name-as-directory
   (expand-file-name (concat tapas-root "../creole-source"))))

(defconst tapas-indexroot
  (file-name-as-directory
   (expand-file-name (concat tapas-root "../indexes"))))

(defconst tapas-embed-handlers
  '(("include" . creole-include-handler)
    ("youtube" . creole-youtube-handler)))

(defun tapas-resolver (name)
  (if (string-match-p "^[A-Za-z-]+" name)
      (concat tapas-docroot name ".creole")
      name))

(defmacro with-creole-embeds (embed-list &rest code)
  (declare (indent 1))
  `(let ((creole-embed-handlers ,embed-list))
     (let ((creole-link-resolver-fn 'tapas-resolver))
       ,@code)))

(defun tapas-make-page (index-file)
  (with-creole-embeds tapas-embed-handlers
    (let ((struct
           (creole-structure
            (creole-tokenize
             (find-file-noselect index-file)))))
      (loop for e in struct
         collect (if (eq 'para (car e))
                     (cons 'para (creole-block-parse (cdr e)))
                     e)))))

(defun tapas-creole->bootstrap (struct)
  "Transform STRUCT, a creole structure, into something bootstrapable.

HTML DIV elements are hacked into the structure wherever we find
an HR element.  The HR elements are retained."
  (noflet ((heading->section-id (heading)
             (format
              "%s-row"
              (replace-regexp-in-string "[ ?]" "-" (cdr heading)))))
    (let ((tx
           (loop for e in struct
              append
                (if (eq (car e) 'heading2)
                    (list
                     `(plugin-html
                       . ,(format
                           "</div></div></div>
<div class=\"section\" id=\"%s\">
<div class=\"container\">
<div class=\"row\">" (heading->section-id e))) e)
                    ;; Else just...
                    (list e)))))
      (append
       '((plugin-html
          . "<div class=\"section\" id=\"sec-top\">
<div class=\"container\">
<div class=\"row\">"))
       tx
       '((plugin-html . "</div></div>"))))))

(defun tapas-creole (page destination)
  (interactive
   (list
    "main.creole"
    (get-buffer-create "*testcreole*")))
  (creole-wiki
   (concat tapas-indexroot page)
   :destination destination
   :structure-transform-fn 'tapas-creole->bootstrap
   :doctype 'html5
   :css (list "/-/bootstrap/css/bootstrap.css"
              "/-/main.css"))
  (when (called-interactively-p 'interactive)
    (switch-to-buffer destination)))

;; Might do for the index page
(defun tapas-main (httpcon)
  (with-creole-embeds tapas-embed-handlers
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (with-stdout-to-elnode httpcon
        (tapas-creole "main.creole" t))))

(defconst tapas-assets-server
  (elnode-webserver-handler-maker
   (file-name-as-directory
    (expand-file-name (concat tapas-root "../assets"))))
  "Webserver for static assets.")

(defun tapas-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]+//$" . tapas-main)
     ("^[^/]+//-/\\(.*\\)" . ,tapas-assets-server))))

(defun tapas-start ()
  (interactive)
  (elnode-start 'tapas-router :port 8006))

;;; tapas.el ends here
