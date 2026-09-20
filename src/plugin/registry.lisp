(in-package #:autolith)

;;;; -- Plugin Manifests --

;;; A plugin is an ordinary ASDF system plus a small manifest. Autolith
;;; discovers manifests from the user data directory and from explicitly
;;; configured paths, then loads each entry system through ASDF. The plugin
;;; never learns whether it came from a mutable user install, a Git checkout,
;;; or an immutable Nix store path.

(defparameter *plugin-manifest-file-name* "plugin.sexp"
  "The manifest file name at the root of every Autolith plugin.")

(defparameter *plugin-configuration-version* 1
  "The plugin configuration file format version accepted by Autolith.")

(defparameter *plugin-manifest-maximum-bytes* (* 64 1024)
  "The maximum byte length of one plugin manifest.")

(defparameter *plugin-name-maximum-characters* 64
  "The maximum character length of one plugin name.")

(defparameter *plugin-version-maximum-characters* 64
  "The maximum character length of one plugin or constraint version.")

(defparameter *plugin-system-maximum-characters* 256
  "The maximum character length of one plugin entry ASDF system name.")

(defparameter *plugin-maximum-plugins* 256
  "The maximum number of plugins discovered in one process.")

(defparameter *plugin-manifest-keys*
  '(:name :version :system :autolith-version :platforms)
  "Every keyword token accepted by a plugin manifest.")

(defparameter *plugin-configuration-keys*
  '(:version :paths :disabled)
  "Every keyword token accepted by the plugin configuration file.")

(defparameter *plugin-platforms*
  '(:linux :macos :windows :bsd :any)
  "The platform keywords a plugin manifest may declare.")

(defparameter *plugin-version-operators*
  '(">=" "<=" ">" "<" "=")
  "The version constraint operators, longest first.")

(define-condition plugin-error (configuration-error)
  ((pathname
    :initarg :pathname
    :initform nil
    :reader plugin-error-pathname
    :type (option pathname)
    :documentation "The plugin manifest or configuration file involved, when any.")
   (plugin-name
    :initarg :plugin-name
    :initform nil
    :reader plugin-error-plugin-name
    :type (option string)
    :documentation "The plugin name involved, when known.")
   (field
    :initarg :field
    :initform nil
    :reader plugin-error-field
    :type (option keyword)
    :documentation "The invalid manifest or configuration field, when known.")
   (cause
    :initarg :cause
    :initform nil
    :reader plugin-error-cause
    :type t
    :documentation "The underlying reader or load failure, when any."))
  (:documentation "A plugin manifest, configuration, or load operation failed."))

(-> plugin--error
    (string &key (:pathname (option pathname))
                 (:plugin-name (option string))
                 (:field (option keyword))
                 (:cause t))
    nil)
(defun plugin--error (message &key pathname plugin-name field cause)
  "Signal a structured plugin failure."
  (error 'plugin-error
         :message message
         :pathname pathname
         :plugin-name plugin-name
         :field field
         :cause cause))


;;;; -- Plugin Records --

(defclass plugin ()
  ((name
    :initarg :name
    :reader plugin-name
    :type non-empty-string
    :documentation "The unique, case-sensitive plugin name.")
   (version
    :initarg :version
    :reader plugin-version
    :type non-empty-string
    :documentation "The plugin's own dotted numeric version.")
   (system
    :initarg :system
    :reader plugin-system
    :type non-empty-string
    :documentation "The entry ASDF system loaded for this plugin.")
   (autolith-version
    :initarg :autolith-version
    :initform nil
    :reader plugin-autolith-version
    :type (option string)
    :documentation "The optional Autolith version constraint, or NIL.")
   (platforms
    :initarg :platforms
    :initform nil
    :reader plugin-platforms
    :type list
    :documentation "The supported platform keywords, or NIL for every platform.")
   (root
    :initarg :root
    :reader plugin-root
    :type pathname
    :documentation "The canonical plugin directory holding the manifest and systems.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader plugin-manifest-pathname
    :type pathname
    :documentation "The manifest file that defined this plugin.")
   (compatible-p
    :initarg :compatible-p
    :initform t
    :accessor plugin-compatible-p
    :type boolean
    :documentation "Whether the Autolith version and host platform satisfy the manifest.")
   (incompatibility
    :initarg :incompatibility
    :initform nil
    :accessor plugin-incompatibility
    :type (option string)
    :documentation "A human-readable reason this plugin cannot load, or NIL.")
   (enabled-p
    :initarg :enabled-p
    :initform t
    :accessor plugin-enabled-p
    :type boolean
    :documentation "Whether startup may load this plugin.")
   (loaded-p
    :initarg :loaded-p
    :initform nil
    :accessor plugin-loaded-p
    :type boolean
    :documentation "Whether this process has loaded the entry system."))
  (:documentation "One discovered Autolith plugin and its lifecycle state."))

(-> plugin-status (plugin) keyword)
(defun plugin-status (plugin)
  "Return PLUGIN's lifecycle status keyword."
  (cond
    ((plugin-loaded-p plugin) ':loaded)
    ((not (plugin-enabled-p plugin)) ':disabled)
    ((not (plugin-compatible-p plugin)) ':incompatible)
    (t ':discovered)))


;;;; -- Version Constraints --

(-> plugin--proper-list-p (t) boolean)
(defun plugin--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (or (null value)
      (and (listp value)
           (handler-case
               (integerp (list-length value))
             (type-error ()
               nil)))))

(-> plugin--version-string-p (t) boolean)
(defun plugin--version-string-p (value)
  "Return true when VALUE is a dotted numeric version such as 0.49.0."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *plugin-version-maximum-characters*)
       (let ((parts (uiop:split-string value :separator ".")))
         (and parts
              (every (lambda (part)
                       (and (plusp (length part))
                            (every #'digit-char-p part)))
                     parts)))))

(-> plugin--version-components (string) list)
(defun plugin--version-components (version)
  "Return VERSION's numeric components in order."
  (mapcar #'parse-integer (uiop:split-string version :separator ".")))

(-> plugin--version-compare (string string) (integer -1 1))
(defun plugin--version-compare (left right)
  "Compare two dotted numeric versions, treating absent components as zero."
  (let* ((left-parts (plugin--version-components left))
         (right-parts (plugin--version-components right))
         (count (max (length left-parts) (length right-parts))))
    (loop for index below count
          for left-part = (or (nth index left-parts) 0)
          for right-part = (or (nth index right-parts) 0)
          when (< left-part right-part)
            do (return-from plugin--version-compare -1)
          when (> left-part right-part)
            do (return-from plugin--version-compare 1)
          finally (return 0))))

(-> plugin--version-constraint-p (t) boolean)
(defun plugin--version-constraint-p (value)
  "Return true when VALUE is a supported version constraint string."
  (and (stringp value)
       (plusp (length value))
       (let* ((operator
                (find-if (lambda (candidate)
                           (uiop:string-prefix-p candidate value))
                         *plugin-version-operators*))
              (target (if operator (subseq value (length operator)) value)))
         (plugin--version-string-p target))))

(-> plugin--version-constraint-satisfied-p (string string) boolean)
(defun plugin--version-constraint-satisfied-p (constraint version)
  "Return true when VERSION satisfies CONSTRAINT.

CONSTRAINT is an optional operator from *PLUGIN-VERSION-OPERATORS* followed by
a dotted numeric version. A bare version means an exact match."
  (let* ((operator
           (find-if (lambda (candidate)
                      (uiop:string-prefix-p candidate constraint))
                    *plugin-version-operators*))
         (target (if operator (subseq constraint (length operator)) constraint))
         (comparison (plugin--version-compare version target)))
    (cond
      ((null operator) (zerop comparison))
      ((string= operator ">=") (>= comparison 0))
      ((string= operator "<=") (<= comparison 0))
      ((string= operator ">") (> comparison 0))
      ((string= operator "<") (< comparison 0))
      ((string= operator "=") (zerop comparison))
      (t nil))))


;;;; -- Manifest Reading --

(-> plugin--read-form (pathname) t)
(defun plugin--read-form (pathname)
  "Read the single top-level form in PATHNAME or signal PLUGIN-ERROR."
  (handler-case
      (multiple-value-bind (form sole-form-p) (snapshot-read pathname)
        (unless sole-form-p
          (plugin--error
           "A plugin file must contain exactly one top-level form."
           :pathname pathname))
        form)
    (plugin-error (condition)
      (error condition))
    (error (cause)
      (plugin--error
       (format nil "Could not read plugin file: ~A" cause)
       :pathname pathname
       :cause cause))))

(-> plugin--validate-plist (t list pathname) list)
(defun plugin--validate-plist (form allowed-keys pathname)
  "Return FORM after validating a proper keyword plist against ALLOWED-KEYS."
  (unless (plugin--proper-list-p form)
    (plugin--error
     "A plugin form must be a proper property list."
     :pathname pathname))
  (unless (evenp (length form))
    (plugin--error
     "A plugin form has a property without a value."
     :pathname pathname))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for tail on form by #'cddr
          for key = (first tail)
          do
             (unless (keywordp key)
               (plugin--error
                (format nil "Plugin key ~S is not a keyword." key)
                :pathname pathname))
             (unless (member key allowed-keys :test #'eq)
               (plugin--error
                (format nil "Unknown plugin key ~S." key)
                :pathname pathname
                :field key))
             (when (gethash key seen)
               (plugin--error
                (format nil "Duplicate plugin key ~S." key)
                :pathname pathname
                :field key))
             (setf (gethash key seen) t)))
  form)

(-> plugin--required-string (list keyword pathname integer) string)
(defun plugin--required-string (form key pathname maximum-characters)
  "Return FORM's required bounded string KEY or signal PLUGIN-ERROR."
  (let ((value (getf form key)))
    (unless (and (stringp value)
                 (plusp (length value))
                 (<= (length value) maximum-characters))
      (plugin--error
       (format nil "Plugin ~S must be a non-empty string of at most ~D characters."
               key maximum-characters)
       :pathname pathname
       :field key))
    value))

(-> plugin--optional-constraint (list pathname) (option string))
(defun plugin--optional-constraint (form pathname)
  "Return FORM's optional :AUTOLITH-VERSION constraint or NIL."
  (let ((value (getf form :autolith-version)))
    (when value
      (unless (plugin--version-constraint-p value)
        (plugin--error
         (format nil "Plugin :AUTOLITH-VERSION ~S is not a valid constraint." value)
         :pathname pathname
         :field ':autolith-version))
      value)))

(-> plugin--platforms (list pathname) list)
(defun plugin--platforms (form pathname)
  "Return FORM's validated :PLATFORMS list, or NIL for every platform."
  (let ((value (getf form :platforms)))
    (cond
      ((null value) nil)
      ((eq value ':any) (list ':any))
      ((plugin--proper-list-p value)
       (unless (and value
                    (every (lambda (platform)
                             (member platform *plugin-platforms* :test #'eq))
                           value)
                    (= (length value)
                       (length (remove-duplicates value :test #'eq))))
         (plugin--error
          "Plugin :PLATFORMS must be a unique list of supported platform keywords."
          :pathname pathname
          :field ':platforms))
       value)
      (t
       (plugin--error
        "Plugin :PLATFORMS must be :ANY or a list of platform keywords."
        :pathname pathname
        :field ':platforms)))))

(-> plugin--manifest (t pathname) list)
(defun plugin--manifest (form pathname)
  "Validate manifest FORM and return a normalized property list."
  (plugin--validate-plist form *plugin-manifest-keys* pathname)
  (let ((name (plugin--required-string
               form :name pathname *plugin-name-maximum-characters*))
        (version (plugin--required-string
                  form :version pathname *plugin-version-maximum-characters*))
        (system (plugin--required-string
                 form :system pathname *plugin-system-maximum-characters*))
        (constraint (plugin--optional-constraint form pathname))
        (platforms (plugin--platforms form pathname)))
    (unless (plugin--version-string-p version)
      (plugin--error
       (format nil "Plugin :VERSION ~S is not a dotted numeric version." version)
       :pathname pathname
       :field ':version))
    (list :name name
          :version version
          :system system
          :autolith-version constraint
          :platforms platforms)))


;;;; -- Plugin Configuration --

(-> configuration-plugins-root (configuration) pathname)
(defun configuration-plugins-root (configuration)
  "Return the user plugin directory beneath CONFIGURATION's data root."
  (merge-pathnames "plugins/" (configuration-data-root configuration)))

(-> configuration-plugins-path (configuration) pathname)
(defun configuration-plugins-path (configuration)
  "Return CONFIGURATION's plugin configuration pathname."
  (merge-pathnames "plugins.sexp" (configuration-config-root configuration)))

(-> plugin--configuration-form (configuration) (option list))
(defun plugin--configuration-form (configuration)
  "Return CONFIGURATION's validated plugin configuration, or NIL when absent."
  (let ((pathname (configuration-plugins-path configuration)))
    (unless (probe-file pathname)
      (return-from plugin--configuration-form nil))
    (let ((form (plugin--read-form pathname)))
      (plugin--validate-plist form *plugin-configuration-keys* pathname)
      (unless (eql (getf form :version) *plugin-configuration-version*)
        (plugin--error
         (format nil "Plugin configuration must use version ~D."
                 *plugin-configuration-version*)
         :pathname pathname
         :field ':version))
      (dolist (key '(:paths :disabled))
        (let ((value (getf form key)))
          (unless (plugin--proper-list-p value)
            (plugin--error
             (format nil "Plugin configuration ~S must be a proper list." key)
             :pathname pathname
             :field key))
          (unless (every (lambda (entry)
                           (and (stringp entry) (plusp (length entry))))
                         value)
            (plugin--error
             (format nil "Plugin configuration ~S must contain non-empty strings."
                     key)
             :pathname pathname
             :field key))))
      form)))

(-> plugin--write-configuration (configuration list) null)
(defun plugin--write-configuration (configuration form)
  "Atomically persist plugin configuration FORM for CONFIGURATION."
  (let ((pathname (configuration-plugins-path configuration)))
    (handler-case
        (progn
          (ensure-directories-exist pathname)
          (snapshot-write pathname form))
      (plugin-error (condition)
        (error condition))
      (error (cause)
        (plugin--error
         (format nil "Could not write plugin configuration: ~A" cause)
         :pathname pathname
         :cause cause))))
  nil)


;;;; -- Discovery --

(-> plugin--canonical-root (pathname) pathname)
(defun plugin--canonical-root (directory)
  "Return DIRECTORY as a canonical directory pathname."
  (uiop:ensure-directory-pathname
   (platform-truename *platform* (uiop:ensure-directory-pathname directory))))

(-> plugin--manifest-pathname (pathname) (option pathname))
(defun plugin--manifest-pathname (directory)
  "Return DIRECTORY's manifest pathname when it exists."
  (let ((pathname (merge-pathnames *plugin-manifest-file-name* directory)))
    (and (probe-file pathname) pathname)))

(-> plugin--roots-in-directory (pathname) list)
(defun plugin--roots-in-directory (directory)
  "Return the plugin roots directly beneath DIRECTORY."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (if (uiop:directory-exists-p directory)
        (loop for child in (uiop:subdirectories directory)
              when (plugin--manifest-pathname child)
                collect (plugin--canonical-root child))
        nil)))

(-> plugin--resolve-configured-path (string) (option pathname))
(defun plugin--resolve-configured-path (value)
  "Return configured path VALUE as an existing canonical directory, or NIL."
  (let* ((expanded (configuration--expanded-working-directory value))
         (pathname
           (uiop:ensure-directory-pathname (platform-pathname expanded))))
    (when (uiop:directory-exists-p pathname)
      (plugin--canonical-root pathname))))

(-> plugin--configured-roots (list) list)
(defun plugin--configured-roots (paths)
  "Return plugin roots named by configured PATHS.

A path that directly holds a manifest is one plugin root; any other existing
directory contributes its immediate plugin subdirectories."
  (loop for value in paths
        for directory = (plugin--resolve-configured-path value)
        when directory
          append (if (plugin--manifest-pathname directory)
                     (list directory)
                     (plugin--roots-in-directory directory))))

(-> plugin--environment-paths () list)
(defun plugin--environment-paths ()
  "Return plugin paths named by the AUTOLITH_PLUGIN_PATH environment variable.

The variable uses the host path separator, so a Nix wrapper or module can
declare plugin store paths without writing a configuration file."
  (mapcar #'namestring (uiop:getenv-pathnames "AUTOLITH_PLUGIN_PATH")))

(-> plugin--compatibility (plugin) (values boolean (option string)))
(defun plugin--compatibility (plugin)
  "Return whether PLUGIN satisfies its version and platform constraints."
  (let ((constraint (plugin-autolith-version plugin)))
    (cond
      ((and constraint
            (not (plugin--version-constraint-satisfied-p
                  constraint *autolith-version*)))
       (values nil
               (format nil "requires Autolith ~A but this is ~A"
                       constraint *autolith-version*)))
      ((not (plugin--platform-supported-p plugin))
       (values nil
               (format nil "does not support ~A"
                       (platform-host-name *platform*))))
      (t (values t nil)))))

(-> plugin--platform-supported-p (plugin) boolean)
(defun plugin--platform-supported-p (plugin)
  "Return true when PLUGIN supports the current host platform."
  (let ((platforms (plugin-platforms plugin)))
    (or (null platforms)
        (not (null (member ':any platforms :test #'eq)))
        (not (null (member (platform-host-name *platform*)
                           platforms
                           :test #'eq))))))

(-> plugin--record (pathname pathname list) plugin)
(defun plugin--record (root manifest-pathname disabled)
  "Return the plugin record described by MANIFEST-PATHNAME beneath ROOT."
  (let* ((manifest (plugin--manifest (plugin--read-form manifest-pathname)
                                     manifest-pathname))
         (plugin
           (make-instance
            'plugin
            :name (getf manifest :name)
            :version (getf manifest :version)
            :system (getf manifest :system)
            :autolith-version (getf manifest :autolith-version)
            :platforms (getf manifest :platforms)
            :root root
            :manifest-pathname manifest-pathname
            :enabled-p
            (not (member (getf manifest :name) disabled :test #'string=)))))
    (multiple-value-bind (compatible-p reason) (plugin--compatibility plugin)
      (setf (plugin-compatible-p plugin) compatible-p
            (plugin-incompatibility plugin) reason))
    plugin))


;;;; -- Registry --

(defvar *plugin-registry* nil
  "The discovered plugin records in discovery order.")

(defvar *plugin-registry-lock* (make-lock "Autolith plugin registry")
  "The lock serializing plugin registry publication and reads.")

(defvar *plugin-configuration* nil
  "The configuration plugin commands use when the caller supplies none.

Application startup sets this to the active configuration so the Lisp prompt
can call PLUGIN-DISCOVER, PLUGIN-REPORT, PLUGIN-ENABLE, and PLUGIN-DISABLE
without arguments.")

(-> plugin--configuration ((option configuration)) configuration)
(defun plugin--configuration (configuration)
  "Return CONFIGURATION, the active plugin configuration, or signal."
  (or configuration
      *plugin-configuration*
      (error 'plugin-error
             :message
             "No plugin configuration is available; start a session or pass one explicitly.")))

(-> plugin--publish (list) null)
(defun plugin--publish (plugins)
  "Replace the published plugin registry with PLUGINS."
  (with-lock-held (*plugin-registry-lock*)
    (setf *plugin-registry* plugins))
  nil)

(-> plugin-list () list)
(defun plugin-list ()
  "Return the discovered plugin registry in discovery order."
  (with-lock-held (*plugin-registry-lock*)
    (copy-list *plugin-registry*)))

(-> plugin-find (string) (option plugin))
(defun plugin-find (name)
  "Return the discovered plugin named NAME, or NIL."
  (find name (plugin-list) :test #'string= :key #'plugin-name))

(-> plugin-report (&optional (option configuration)) string)
(defun plugin-report (&optional configuration)
  "Discover plugins and return a human-readable report string.

This is the prompt-facing inspection command; PLUGIN-LIST returns the records."
  (let ((plugins (plugin-discover configuration)))
    (if (null plugins)
        "No plugins discovered."
        (format nil "~{~A~^~%~}"
                (mapcar (lambda (plugin)
                          (format nil "~A ~A [~(~A~)] ~A"
                                  (plugin-name plugin)
                                  (plugin-version plugin)
                                  (plugin-status plugin)
                                  (namestring (plugin-root plugin))))
                        plugins)))))

(-> plugin-discover (&optional (option configuration)) list)
(defun plugin-discover (&optional configuration)
  "Discover CONFIGURATION's plugins and publish the registry.

CONFIGURATION defaults to the active plugin configuration. The user data
plugin directory is scanned first, followed by AUTOLITH_PLUGIN_PATH entries
and then explicitly configured paths. Malformed manifests and duplicate names
signal PLUGIN-ERROR."
  (let* ((configuration (plugin--configuration configuration))
         (form (plugin--configuration-form configuration))
         (disabled (getf form :disabled))
         (paths (append (plugin--environment-paths) (getf form :paths)))
         (roots
           (remove-duplicates
            (append (plugin--roots-in-directory
                     (configuration-plugins-root configuration))
                    (plugin--configured-roots paths))
            :test #'equal))
         (seen (make-hash-table :test #'equal))
         (plugins nil))
    (when (> (length roots) *plugin-maximum-plugins*)
      (plugin--error
       (format nil "Plugin discovery exceeds the limit of ~D plugins."
               *plugin-maximum-plugins*)
       :pathname (configuration-plugins-root configuration)))
    (dolist (root roots)
      (let* ((manifest-pathname (plugin--manifest-pathname root))
             (plugin (plugin--record root manifest-pathname disabled)))
        (when (gethash (plugin-name plugin) seen)
          (plugin--error
           (format nil "Duplicate plugin name ~S." (plugin-name plugin))
           :pathname manifest-pathname
           :plugin-name (plugin-name plugin)))
        (setf (gethash (plugin-name plugin) seen) t)
        (push plugin plugins)))
    (setf plugins (nreverse plugins))
    (plugin--publish plugins)
    plugins))


;;;; -- Lifecycle --

(-> plugin-load (plugin) plugin)
(defun plugin-load (plugin)
  "Load PLUGIN's entry ASDF system and return PLUGIN.

A disabled or incompatible plugin is refused. Loading is idempotent: an
already-loaded plugin is returned unchanged."
  (when (plugin-loaded-p plugin)
    (return-from plugin-load plugin))
  (unless (plugin-enabled-p plugin)
    (plugin--error
     (format nil "Plugin ~A is disabled." (plugin-name plugin))
     :pathname (plugin-manifest-pathname plugin)
     :plugin-name (plugin-name plugin)))
  (unless (plugin-compatible-p plugin)
    (plugin--error
     (format nil "Plugin ~A is incompatible: ~A"
             (plugin-name plugin)
             (plugin-incompatibility plugin))
     :pathname (plugin-manifest-pathname plugin)
     :plugin-name (plugin-name plugin)))
  (let ((root (plugin-root plugin)))
    (pushnew root asdf:*central-registry* :test #'equal)
    (handler-case
        (asdf:load-system (plugin-system plugin))
      (error (cause)
        (plugin--error
         (format nil "Could not load plugin ~A system ~A: ~A"
                 (plugin-name plugin)
                 (plugin-system plugin)
                 cause)
         :pathname (plugin-manifest-pathname plugin)
         :plugin-name (plugin-name plugin)
         :cause cause)))
    (setf (plugin-loaded-p plugin) t))
  plugin)

(-> plugin-load-all (&optional (option configuration)) list)
(defun plugin-load-all (&optional configuration)
  "Discover and load every enabled, compatible plugin for CONFIGURATION.

CONFIGURATION defaults to the active plugin configuration. Return the
published registry. Disabled and incompatible plugins are left unloaded and
remain visible through PLUGIN-LIST."
  (let ((plugins (plugin-discover configuration)))
    (dolist (plugin plugins)
      (pushnew (plugin-root plugin) asdf:*central-registry* :test #'equal))
    (dolist (plugin plugins)
      (when (and (plugin-enabled-p plugin) (plugin-compatible-p plugin))
        (plugin-load plugin)))
    (plugin-list)))

(-> plugin--set-enabled (configuration string boolean) boolean)
(defun plugin--set-enabled (configuration name enabled-p)
  "Persist NAME's enabled state and update the published registry."
  (let* ((form (or (plugin--configuration-form configuration)
                   (list :version *plugin-configuration-version*)))
         (disabled (remove name (getf form :disabled) :test #'string=)))
    (unless enabled-p
      (setf disabled (append disabled (list name))))
    (setf (getf form :disabled) disabled)
    (plugin--write-configuration configuration form)
    (let ((plugin (plugin-find name)))
      (when plugin
        (setf (plugin-enabled-p plugin) enabled-p)))
    enabled-p))

(-> plugin-enable (string &optional (option configuration)) boolean)
(defun plugin-enable (name &optional configuration)
  "Enable plugin NAME for future loads and persist the change.

CONFIGURATION defaults to the active plugin configuration."
  (plugin--set-enabled (plugin--configuration configuration) name t))

(-> plugin-disable (string &optional (option configuration)) boolean)
(defun plugin-disable (name &optional configuration)
  "Disable plugin NAME for future loads and persist the change.

CONFIGURATION defaults to the active plugin configuration."
  (plugin--set-enabled (plugin--configuration configuration) name nil))
