(asdf:defsystem #:autolith-plugin-hello
  :description "Example Autolith plugin."
  :author "Autolith contributors"
  :license "ISC"
  :version "0.1.0"
  :depends-on (#:autolith)
  :serial t
  :components ((:file "hello")))
