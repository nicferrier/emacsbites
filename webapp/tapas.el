;;; tapas.el -- a webapp presenting the emacslisp tapas -*- lexical-binding: t -*-

(require 'elnode)

(elnode-app tapas-root creole noflet cl)

(defconst tapas-docroot
  (file-name-as-directory
   (expand-file-name (concat tapas-root "../creole-source"))))

(defconst tapas-indexroot
  (file-name-as-directory
   (expand-file-name (concat tapas-root "../indexes"))))

(defconst tapas-embed-handlers
  '(("include" . creole-summary-handler)
    ("youtube" . creole-youtube-handler)))

(defconst tapas-licence-badge
  "<div class=\"cc\">
<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/deed.en_GB\"><img alt=\"Creative Commons Licence\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></img></a><span xmlns:dct=\"http://purl.org/dc/terms/\" property=\"dct:title\">emacs tapas</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://nic.ferrier.me.uk\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">nic ferier</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/deed.en_GB\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.
</div>"
  "The Creative Commons plugin code.")

(defconst tapas-ga "<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-24502836-7', 'emacstapas.com');
  ga('send', 'pageview');

</script>"
  "The tapas GA script.")

(defconst tapas-tweet
  "<a href=\"https://twitter.com/share\"
class=\"twitter-share-button\"
data-url=\"http://emacstapas.com\"
data-via=\"emacstapas\"
data-size=\"large\" data-hashtags=\"emacs\">Tweet</a>
<script>
// <!--
!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');
// -->
</script>"
  "The text of the tweet button.")

(defun tapas-resolver (name)
  (let ((rooted (concat tapas-docroot name ".creole")))
    (if (file-exists-p rooted)
        rooted
        name)))

(defun tapas-get-file (filename)
  "Stop worrying about `revert-without-query'."
  (with-current-buffer (generate-new-buffer)
    (insert-file-contents filename)
    (current-buffer)))

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
             (tapas-get-file index-file)))))
      (loop for e in struct
         collect (if (eq 'para (car e))
                     (cons 'para (creole-block-parse (cdr e)))
                     e)))))

(defvar tapas/struct-class :other
  "Dynamic variable for passing the class of page.

The value should be one of `:other', `:episode', `:series' or
`:main'.")

(defun tapas/creole-struct (struct)
  (noflet ((heading->section-id (heading)
             (format
              "%s-row"
              (replace-regexp-in-string
               "[ ?]" "-" (cdr heading)))))
    (let* ((even t)
           (tx
            (loop for e in struct
               do (setq even (not even))
               append
                 (if (eq (car e) 'heading2)
                     (list
                      `(plugin-html
                        . ,(s-format
                            "</div></div></div>
<div class=\"section ${even}\" id=\"${section}\">
<div class=\"container\">
<div class=\"row\">" 'aget `(("even" . ,(if even "even" ""))
                             ("section" . ,(heading->section-id e))))) e)
                     ;; Else just...
                     (list e)))))
      tx)))

(defun tapas-creole->bootstrap (struct)
  "Transform STRUCT, a creole structure, into something bootstrapable.

HTML DIV elements are hacked into the structure wherever we find
an HR element.  The HR elements are retained."
  (let ((tx (tapas/creole-struct struct)))
    (append
     `((plugin-html
        . ,(concat
            (when (member tapas/struct-class '(:episode :series))
              (concat
               "<a id=\"homelink\" href=\"/\">emacs tapas</a>"
               tapas-tweet))
            "<div class=\"section\" id=\"sec-top\">
<div class=\"container\">
<div class=\"row\">")))
     tx
     `((plugin-html . "</div></div></div><footer>")
       (ul
        "(C) 2013 Nic Ferrier"
        "[[http://www.emacstapas.com/terms|terms]]"
        "[[http://www.emacstapas.com/contact|contact]]")
       (plugin-html . ,tapas-licence-badge)
       (plugin-html . ,tapas-ga)
       (plugin-html . "</footer>")))))

(defun tapas-creole (creole-file destination &optional css)
  "Abstract the creole rendering to HTML a little."
  (interactive
   (list
    (concat tapas-indexroot "main.creole")
    (get-buffer-create "*testcreole*")))
  (creole-wiki
   creole-file
   :destination destination
   :structure-transform-fn 'tapas-creole->bootstrap
   :doctype 'html5
   :css (list "/-/bootstrap/css/bootstrap.css"
              "/-/common.css"
              (case css
                (:main "/-/main.css")
                (:index "/-/index.css")
                (t "/-/episode.css"))))
  (when (called-interactively-p 'interactive)
    (switch-to-buffer destination)))

(defun tapas-creole-page (httpcon filename &optional css)
  (with-creole-embeds tapas-embed-handlers
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (with-stdout-to-elnode httpcon
        (tapas-creole filename t css))))

(defun tapas-episode (httpcon)
  "Episode server."
  (noflet ((elnode-http-mapping (httpcon which)
             (concat (funcall this-fn httpcon 1) ".creole")))
    (let ((tapas/struct-class :series))
      (elnode-docroot-for tapas-docroot
          with targetfile
          on httpcon
          do
          (tapas-creole-page httpcon targetfile)))))

(defun tapas-series (httpcon)
  "Index server."
  (noflet ((elnode-http-mapping (httpcon which)
             (concat (funcall this-fn httpcon 1) ".creole")))
    (let ((tapas/struct-class :series)
          (creole-youtube-handler-width 266)
          (creole-youtube-handler-height 200)
          (creole-summary-resolver
           (lambda (path)
             (format "/episode/%s" path))))
      (elnode-docroot-for tapas-indexroot
          with targetfile
          on httpcon
          do
          (tapas-creole-page httpcon targetfile)))))

;; Might do for the index page
(defun tapas-main (httpcon)
  (let ((tapas/struct-class :main)
        (page (concat tapas-indexroot "main.creole")))
    (tapas-creole-page httpcon page :main)))

(defun tapas/make-creole (filename)
  (lambda (httpcon)
    (let ((tapas/struct-class :other)
          (page filename))
      (tapas-creole-page httpcon page :main))))

(defconst tapas-assets-server
  (elnode-webserver-handler-maker
   (file-name-as-directory
    (expand-file-name (concat tapas-root "../assets"))))
  "Webserver for static assets.")

(defun tapas-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]+//$" . tapas-main)
     ("^[^/]+//favicon.ico"
      . ,(elnode-make-send-file
          (concat tapas-root "../assets/olive.ico")))
     ("^[^/]+//-/\\(.*\\)" . ,tapas-assets-server)
     ("^[^/]+//terms$" . ,(tapas/make-creole
                           (concat tapas-docroot "terms.creole")))
     ("^[^/]+//contact$" . ,(tapas/make-creole
                             (concat tapas-docroot "contact.creole")))
     ("^[^/]+//series/\\(.*\\)" . tapas-series)
     ("^[^/]+//episode/\\(.*\\)" . tapas-episode))))

(defun tapas-start ()
  (interactive)
  (elnode-start 'tapas-router :port 8006))

;;; tapas.el ends here
