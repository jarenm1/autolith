(in-package #:autolith)

;;;; -- Plugin Tests --

(defparameter *plugin-test-malformed-manifests*
  '(("(:name \"bad\" :version \"0.1.0\")"
     . "a manifest without an entry system is rejected")
    ("(:name \"bad\" :version \"0.1.0\" :system \"x\" :unknown 1)"
     . "a manifest with an unknown key is rejected")
    ("(:name \"bad\" :version \"nope\" :system \"x\")"
     . "a manifest with a non-numeric version is rejected")
    ("(:name \"bad\" :version \"0.1.0\" :system \"x\" :autolith-version \"~1\")"
     . "a manifest with an invalid version constraint is rejected")
    ("(:name \"bad\" :version \"0.1.0\" :system \"x\" :platforms (:plan9))"
     . "a manifest with an unknown platform is rejected")
    ("(:name \"bad\" :version \"0.1.0\" :system \"x\" :name \"dup\")"
     . "a manifest with a duplicate key is rejected")
    ("not-a-list"
     . "a manifest that is not a property list is rejected")
    ("(:name \"bad\" :version \"0.1.0\" :system)"
     . "a manifest with a dangling property is rejected"))
  "Malformed plugin manifests and the reason each must be rejected.")

(defparameter *plugin-test-version-constraints*
  '((">=0.49.0" "0.49.0" t)
    (">=0.49.0" "0.50.0" t)
    (">=0.49.0" "0.48.0" nil)
    (">0.49.0" "0.49.0" nil)
    ("<=0.49.0" "0.49.0" t)
    ("<0.49.0" "0.49.0" nil)
    ("=0.49.0" "0.49.0" t)
    ("=0.49.0" "0.49.1" nil)
    ("0.49.0" "0.49.0" t)
    ("0.49.0" "0.49" t)
    (">=0.49" "0.49.0" t)
    (">=0.49" "0.48.9" nil))
  "Version constraints, candidate versions, and expected satisfaction.")

(defparameter *plugin-test-version-comparisons*
  '(("0.49.0" "0.49.0" 0)
    ("0.49.0" "0.49" 0)
    ("0.50.0" "0.49.9" 1)
    ("0.49.0" "0.50.0" -1))
  "Version pairs and their expected ordering.")

(-> plugin-test--write-file (pathname string) pathname)
(defun plugin-test--write-file (pathname content)
  "Write CONTENT to PATHNAME, creating parent directories."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create)
    (write-string content stream))
  pathname)

(-> plugin-test--install
    (pathname &key (:name string) (:version string) (:system string)
                  (:autolith-version (option string)) (:platforms list)
                  (:identifier string))
    pathname)
(defun plugin-test--install
    (directory &key name (version "0.1.0") system autolith-version platforms
                 (identifier "plugin-test-hello"))
  "Write a complete plugin into DIRECTORY and return it.

The entry system depends on Autolith and registers one context contributor
under IDENTIFIER, exercising the full discovery and load path."
  (ensure-directories-exist directory)
  (let ((manifest (list :name name :version version :system system)))
    (when autolith-version
      (setf (getf manifest :autolith-version) autolith-version))
    (when platforms
      (setf (getf manifest :platforms) platforms))
    (plugin-test--write-file (merge-pathnames "plugin.sexp" directory)
                             (format nil "~S~%" manifest)))
  (plugin-test--write-file
   (merge-pathnames (format nil "~A.asd" system) directory)
   (format nil "(asdf:defsystem #:~A~%  :depends-on (#:autolith)~%  :serial t~%  :components ((:file \"hello\")))~%"
           system))
  (plugin-test--write-file
   (merge-pathnames "hello.lisp" directory)
   (format nil "(in-package #:autolith)~%(register-context-contributor ~S (lambda (request) (declare (ignore request)) nil) :source :plugin)~%"
           identifier))
  directory)

(-> plugin-test--plugin-root (configuration string) pathname)
(defun plugin-test--plugin-root (configuration name)
  "Return the user plugin directory named NAME for CONFIGURATION."
  (merge-pathnames (format nil "~A/" name)
                   (configuration-plugins-root configuration)))

(-> test-plugin-discovery () boolean)
(defun test-plugin-discovery ()
  "Discovery reads a manifest and reports its identity and status."
  (with-test-configuration (configuration)
    (let* ((system "autolith-plugin-test-discovery")
           (root (plugin-test--plugin-root configuration "hello")))
      (plugin-test--install root :name "hello" :system system)
      (let ((plugins (plugin-discover configuration)))
        (test-assert (= (length plugins) 1) "one plugin is discovered")
        (let ((plugin (first plugins)))
          (test-assert (string= (plugin-name plugin) "hello")
                       "the plugin name is read")
          (test-assert (string= (plugin-version plugin) "0.1.0")
                       "the plugin version is read")
          (test-assert (string= (plugin-system plugin) system)
                       "the entry system is read")
          (test-assert (plugin-compatible-p plugin)
                       "a compatible plugin is marked compatible")
          (test-assert (eq (plugin-status plugin) ':discovered)
                       "a discovered plugin is not yet loaded")
          (test-assert (search "hello" (plugin-report configuration))
                       "the report names the discovered plugin"))))))

(-> test-plugin-malformed-manifest () boolean)
(defun test-plugin-malformed-manifest ()
  "Every malformed manifest is rejected with a structured plugin error."
  (with-test-configuration (configuration)
    (loop for (form . description) in *plugin-test-malformed-manifests*
          for root = (plugin-test--plugin-root
                      configuration (format nil "bad-~D" (random 1000000)))
          do (ensure-directories-exist root)
             (plugin-test--write-file (merge-pathnames "plugin.sexp" root)
                                      (format nil "~A~%" form))
             (test-assert
              (handler-case
                  (progn (plugin-discover configuration) nil)
                (plugin-error () t))
              description))))

(-> test-plugin-duplicate-names () boolean)
(defun test-plugin-duplicate-names ()
  "Two plugins sharing a name are rejected during discovery."
  (with-test-configuration (configuration)
    (plugin-test--install (plugin-test--plugin-root configuration "first")
                          :name "duplicate"
                          :system "autolith-plugin-test-duplicate-first")
    (plugin-test--install (plugin-test--plugin-root configuration "second")
                          :name "duplicate"
                          :system "autolith-plugin-test-duplicate-second")
    (test-assert
     (handler-case
         (progn (plugin-discover configuration) nil)
       (plugin-error () t))
     "duplicate plugin names are rejected")))

(-> test-plugin-incompatible-version () boolean)
(defun test-plugin-incompatible-version ()
  "A plugin requiring a newer Autolith is discovered but refused."
  (with-test-configuration (configuration)
    (plugin-test--install (plugin-test--plugin-root configuration "future")
                          :name "future"
                          :system "autolith-plugin-test-future"
                          :autolith-version ">=99.0.0")
    (let ((plugin (first (plugin-discover configuration))))
      (test-assert (not (plugin-compatible-p plugin))
                   "an unsatisfied version constraint is incompatible")
      (test-assert (eq (plugin-status plugin) ':incompatible)
                   "the status reports incompatibility")
      (test-assert
       (handler-case
           (progn (plugin-load plugin) nil)
         (plugin-error () t))
       "loading an incompatible plugin is refused"))))

(-> test-plugin-incompatible-platform () boolean)
(defun test-plugin-incompatible-platform ()
  "A plugin that does not support this host is discovered but refused."
  (with-test-configuration (configuration)
    (let ((other (if (eq (platform-host-name *platform*) ':windows)
                     ':linux
                     ':windows)))
      (plugin-test--install (plugin-test--plugin-root configuration "other")
                            :name "other"
                            :system "autolith-plugin-test-other"
                            :platforms (list other))
      (plugin-test--install (plugin-test--plugin-root configuration "native")
                            :name "native"
                            :system "autolith-plugin-test-native"
                            :platforms (list (platform-host-name *platform*)))
      (let* ((plugins (plugin-discover configuration))
             (other-plugin (plugin-find "other"))
             (native-plugin (plugin-find "native")))
        (test-assert (= (length plugins) 2) "both plugins are discovered")
        (test-assert (not (plugin-compatible-p other-plugin))
                     "an unsupported platform is incompatible")
        (test-assert (plugin-compatible-p native-plugin)
                     "a supported platform is compatible")
        (test-assert
         (handler-case
             (progn (plugin-load other-plugin) nil)
           (plugin-error () t))
         "loading a plugin for another platform is refused")))))

(-> test-plugin-version-constraints () boolean)
(defun test-plugin-version-constraints ()
  "Version constraints and comparisons follow dotted numeric ordering."
  (loop for (constraint version expected) in *plugin-test-version-constraints*
        do (test-assert
            (eq (plugin--version-constraint-satisfied-p constraint version)
                expected)
            (format nil "~A satisfies ~A is ~A" version constraint expected)))
  (loop for (left right expected) in *plugin-test-version-comparisons*
        do (test-assert
            (= (plugin--version-compare left right) expected)
            (format nil "~A compares to ~A as ~A" left right expected))))

(-> test-plugin-load-from-user-path () boolean)
(defun test-plugin-load-from-user-path ()
  "A plugin in the user data directory loads and registers through ASDF."
  (with-test-configuration (configuration)
    (let ((identifier "plugin-test-user-load"))
      (plugin-test--install (plugin-test--plugin-root configuration "hello")
                            :name "hello"
                            :system "autolith-plugin-test-user-load"
                            :identifier identifier)
      (unwind-protect
           (let ((plugins (plugin-load-all configuration)))
             (test-assert (= (length plugins) 1) "one plugin is discovered")
             (test-assert (plugin-loaded-p (first plugins))
                          "the plugin entry system is loaded")
             (test-assert (eq (plugin-status (first plugins)) ':loaded)
                          "the status reports the loaded plugin")
             (test-assert (not (null (context--registration-find identifier)))
                          "the plugin registered its contributor"))
        (unregister-context-contributor identifier)))))

(-> test-plugin-load-from-immutable-path () boolean)
(defun test-plugin-load-from-immutable-path ()
  "A plugin named by an explicit path loads exactly like a user install."
  (with-test-configuration (configuration)
    (let* ((identifier "plugin-test-immutable-load")
           (immutable (merge-pathnames "immutable/"
                                       (test-configuration-root configuration))))
      (plugin-test--install immutable
                            :name "immutable"
                            :system "autolith-plugin-test-immutable"
                            :identifier identifier)
      (plugin-test--write-file
       (configuration-plugins-path configuration)
       (format nil "(:version 1 :paths (~S))~%" (namestring immutable)))
      (unwind-protect
           (let ((plugins (plugin-load-all configuration)))
             (test-assert (= (length plugins) 1)
                          "the configured plugin is discovered")
             (test-assert (string= (plugin-name (first plugins)) "immutable")
                          "the configured plugin name is read")
             (test-assert (plugin-loaded-p (first plugins))
                          "the configured plugin entry system is loaded")
             (test-assert (not (null (context--registration-find identifier)))
                          "the configured plugin registered its contributor"))
        (unregister-context-contributor identifier)))))

(-> test-plugin-enable-disable () boolean)
(defun test-plugin-enable-disable ()
  "Disabling a plugin persists and prevents loading until it is enabled."
  (with-test-configuration (configuration)
    (plugin-test--install (plugin-test--plugin-root configuration "hello")
                          :name "hello"
                          :system "autolith-plugin-test-toggle")
    (plugin-discover configuration)
    (plugin-disable "hello" configuration)
    (test-assert (not (plugin-enabled-p (plugin-find "hello")))
                 "the plugin is disabled")
    (let ((plugins (plugin-load-all configuration)))
      (test-assert (not (plugin-loaded-p (first plugins)))
                   "a disabled plugin is not loaded")
      (test-assert (eq (plugin-status (first plugins)) ':disabled)
                   "the status reports the disabled plugin"))
    (plugin-enable "hello" configuration)
    (test-assert (plugin-enabled-p (plugin-find "hello"))
                 "the plugin is enabled again")))

(-> test-plugin-active-configuration () boolean)
(defun test-plugin-active-configuration ()
  "The prompt-facing commands default to the active plugin configuration."
  (with-test-configuration (configuration)
    (plugin-test--install (plugin-test--plugin-root configuration "hello")
                          :name "hello"
                          :system "autolith-plugin-test-active")
    (let ((*plugin-configuration* configuration))
      (test-assert (= (length (plugin-discover)) 1)
                   "discover defaults to the active configuration")
      (test-assert (search "hello" (plugin-report))
                   "report defaults to the active configuration")
      (plugin-disable "hello")
      (test-assert (not (plugin-enabled-p (plugin-find "hello")))
                   "disable defaults to the active configuration")
      (plugin-enable "hello")
      (test-assert (plugin-enabled-p (plugin-find "hello"))
                   "enable defaults to the active configuration"))))

(-> test-plugin-environment-path () boolean)
(defun test-plugin-environment-path ()
  "AUTOLITH_PLUGIN_PATH names plugin roots without a configuration file."
  (with-test-configuration (configuration)
    (let* ((identifier "plugin-test-environment-load")
           (root (merge-pathnames "environment/"
                                  (test-configuration-root configuration))))
      (plugin-test--install root
                            :name "environment"
                            :system "autolith-plugin-test-environment"
                            :identifier identifier)
      (unwind-protect
           (with-test-environment
               (("AUTOLITH_PLUGIN_PATH" (namestring root)))
             (let ((plugins (plugin-load-all configuration)))
               (test-assert (= (length plugins) 1)
                            "the environment plugin is discovered")
               (test-assert (string= (plugin-name (first plugins)) "environment")
                            "the environment plugin name is read")
               (test-assert (plugin-loaded-p (first plugins))
                            "the environment plugin entry system is loaded")
               (test-assert (not (null (context--registration-find identifier)))
                            "the environment plugin registered its contributor")))
        (unregister-context-contributor identifier)))))
