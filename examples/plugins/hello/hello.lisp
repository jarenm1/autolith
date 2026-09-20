(in-package #:autolith)

;;;; -- Example Plugin --

;;; This file is the entire behavior of the example plugin. Loading the
;;; entry system runs this top-level form, which registers one request
;;; context contributor through the ordinary Autolith extension API. The
;;; plugin never learns whether it was loaded from a user directory or an
;;; immutable Nix store path.

(define-context-contributor hello-plugin-context (request)
  "Contribute a fixed greeting as the example plugin's only behavior."
  (declare (ignore request))
  (make-context-contribution
   :identifier "hello-plugin-greeting"
   :instruction "The hello example plugin is loaded."
   :priority -500))
