;;; bites.el -- a webapp presenting the emacslisp bites -*- lexical-binding: t -*-

(require 'elnode)

(elnode-app bites-root creole noflet cl)

(defconst bites-docroot
  (file-name-as-directory
   (expand-file-name (concat bites-root "../creole-source"))))

(defconst bites-indexroot
  (file-name-as-directory
   (expand-file-name (concat bites-root "../indexes"))))

(defconst bites-embed-handlers
  '(("include" . creole-summary-handler)
    ("youtube" . creole-youtube-handler)))

(defconst bites-licence-badge
  "<div class=\"cc\">
<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/deed.en_GB\"><img alt=\"Creative Commons Licence\" style=\"border-width:0\" src=\"http://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></img></a><span xmlns:dct=\"http://purl.org/dc/terms/\" property=\"dct:title\">emacsbites</span> by <a xmlns:cc=\"http://creativecommons.org/ns#\" href=\"http://nic.ferrier.me.uk\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">nic ferrier</a> is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/3.0/deed.en_GB\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.
</div>"
  "The Creative Commons plugin code.")

(defconst bites-ga "<script>
// <!--
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
  ga('create', 'UA-24502836-8', 'emacsbites.com');
  ga('send', 'pageview');

// -->
</script>"
  "The bites GA script.")

(defconst bites-tweet
  "<a href=\"https://twitter.com/share\"
class=\"twitter-share-button\"
data-url=\"http://emacsbites.com\"
data-via=\"emacsbites\"
data-size=\"large\" data-hashtags=\"emacs\">Tweet</a>
<script>
// <!--
!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');
// -->
</script>"
  "The text of the tweet button.")

(defun bites-resolver (name)
  (let ((rooted (concat bites-docroot name ".creole")))
    (if (file-exists-p rooted)
        rooted
        name)))

(defun bites-get-file (filename)
  "Stop worrying about `revert-without-query'."
  (with-current-buffer (generate-new-buffer)
    (insert-file-contents filename)
    (current-buffer)))

(defmacro with-creole-embeds (embed-list &rest code)
  (declare (indent 1))
  `(let ((creole-embed-handlers ,embed-list))
     (let ((creole-link-resolver-fn 'bites-resolver))
       ,@code)))

(defun bites-make-page (index-file)
  (with-creole-embeds bites-embed-handlers
    (let ((struct
           (creole-structure
            (creole-tokenize
             (bites-get-file index-file)))))
      (loop for e in struct
         collect (if (eq 'para (car e))
                     (cons 'para (creole-block-parse (cdr e)))
                     e)))))

(defvar bites/struct-class :other
  "Dynamic variable for passing the class of page.

The value should be one of `:other', `:episode', `:series' or
`:main'.")

(defun bites/creole-struct (struct)
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

(defun bites-creole->bootstrap (struct)
  "Transform STRUCT, a creole structure, into something bootstrapable.

HTML DIV elements are hacked into the structure wherever we find
an HR element.  The HR elements are retained."
  (let ((tx (bites/creole-struct struct)))
    (append
     `((plugin-html
        . ,(concat
            (when (member bites/struct-class '(:episode :series))
              (concat
               "<a id=\"homelink\" href=\"/\">emacs bites</a>"
               bites-tweet))
            "<div class=\"section\" id=\"sec-top\">
<div class=\"container\">
<div class=\"row\">")))
     tx
     `((plugin-html . "</div></div></div><footer>")
       (ul
        "(C) 2013 Nic Ferrier"
        "[[http://www.emacsbites.com/terms|terms]]"
        "[[http://www.emacsbites.com/contact|contact]]")
       (plugin-html . ,bites-licence-badge)
       (plugin-html . ,bites-ga)
       (plugin-html . "</footer>")))))

(defun bites-creole (creole-file destination &optional css)
  "Abstract the creole rendering to HTML a little."
  (interactive
   (list
    (concat bites-indexroot "main.creole")
    (get-buffer-create "*testcreole*")))
  (creole-wiki
   creole-file
   :destination destination
   :structure-transform-fn 'bites-creole->bootstrap
   :doctype 'html5
   :css (list "/-/bootstrap/css/bootstrap.css"
              "/-/common.css"
              (case css
                (:main "/-/main.css")
                (:index "/-/index.css")
                (t "/-/episode.css"))))
  (when (called-interactively-p 'interactive)
    (switch-to-buffer destination)))

(defun bites-creole-page (httpcon filename &optional css)
  (with-creole-embeds bites-embed-handlers
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (with-stdout-to-elnode httpcon
        (bites-creole filename t css))))

(defun bites-episode (httpcon)
  "Episode server."
  (noflet ((elnode-http-mapping (httpcon which)
             (concat (funcall this-fn httpcon 1) ".creole")))
    (let ((bites/struct-class :series))
      (elnode-docroot-for bites-docroot
          with targetfile
          on httpcon
          do
          (bites-creole-page httpcon targetfile)))))

(defun bites-series (httpcon)
  "Index server."
  (noflet ((elnode-http-mapping (httpcon which)
             (concat (funcall this-fn httpcon 1) ".creole")))
    (let ((bites/struct-class :series)
          (creole-youtube-handler-width 266)
          (creole-youtube-handler-height 200)
          (creole-summary-resolver
           (lambda (path)
             (format "/episode/%s" path))))
      (elnode-docroot-for bites-indexroot
          with targetfile
          on httpcon
          do
          (bites-creole-page httpcon targetfile)))))

;; Might do for the index page
(defun bites-main (httpcon)
  (let ((bites/struct-class :main)
        (page (concat bites-indexroot "main.creole")))
    (bites-creole-page httpcon page :main)))

(defun bites/make-creole (filename)
  (lambda (httpcon)
    (let ((bites/struct-class :other)
          (page filename))
      (bites-creole-page httpcon page :main))))

(defconst bites-assets-server
  (elnode-webserver-handler-maker
   (file-name-as-directory
    (expand-file-name (concat bites-root "../assets"))))
  "Webserver for static assets.")

(defun bites-router (httpcon)
  (elnode-hostpath-dispatcher
   httpcon
   `(("^[^/]+//$" . bites-main)
     ("^[^/]+//favicon.ico"
      . ,(elnode-make-send-file
          (concat bites-root "../assets/olive.ico")))
     ("^[^/]+//-/\\(.*\\)" . ,bites-assets-server)
     ("^[^/]+//terms$" . ,(bites/make-creole
                           (concat bites-docroot "terms.creole")))
     ("^[^/]+//contact$" . ,(bites/make-creole
                             (concat bites-docroot "contact.creole")))
     ("^[^/]+//series/\\(.*\\)" . bites-series)
     ("^[^/]+//episode/\\(.*\\)" . bites-episode))))

(defun bites-start ()
  (interactive)
  (elnode-start 'bites-router :port 8006))

;;; bites.el ends here
