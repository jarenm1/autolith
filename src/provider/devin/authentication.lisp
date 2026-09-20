(in-package #:autolith)

;;;; -- Devin OAuth Login --

;;; Devin CLI authenticates through a browser PKCE code flow and stores the
;;; resulting token. This file implements that flow for Autolith and adapts the
;;; token to Autolith's credential store. The flow mirrors the Devin CLI login
;;; inspected in the oh-my-pi reference checkout.

(defparameter *devin-authorize-url* "https://app.devin.ai/auth/cli/continue"
  "The Devin CLI browser authorization endpoint.")

(defparameter *devin-token-url* "https://api.devin.ai/auth/cli/token"
  "The Devin CLI token exchange endpoint.")

(defparameter *devin-api-endpoint* "https://api.devin.ai"
  "The Devin API endpoint recorded with the credential.")

(defparameter *devin-oauth-callback-port* 59653
  "The loopback port the Devin CLI login callback listens on.")

(defparameter *devin-oauth-callback-path* "/callback"
  "The loopback path the Devin CLI login callback uses.")

(defparameter *devin-oauth-callback-timeout* 900
  "The maximum seconds to wait for the Devin browser callback.")

(defparameter *devin-oauth-request-timeout* 5
  "The maximum seconds allowed to read one local callback request line.")

(defparameter *devin-oauth-request-line-limit* 8192
  "The maximum characters accepted in one local callback request line.")

(define-condition devin-oauth-error (authentication-error)
  ((stage
    :initarg :stage
    :reader devin-oauth-error-stage
    :type keyword
    :documentation "The browser OAuth stage that failed.")
   (status
    :initarg :status
    :initform nil
    :reader devin-oauth-error-status
    :type (option integer)
    :documentation "The HTTP status returned by Devin, if known.")
   (code
    :initarg :code
    :initform nil
    :reader devin-oauth-error-code
    :type (option string)
    :documentation "A bounded non-secret OAuth error code, if supplied."))
  (:documentation "A failure in Devin browser OAuth."))

(-> devin-oauth--fail
    (keyword string &key (:status (option integer)) (:code (option string)))
    nil)
(defun devin-oauth--fail (stage message &key status code)
  "Signal a structured Devin OAuth failure containing only safe metadata."
  (error 'devin-oauth-error
         :message message
         :stage stage
         :status status
         :code code))

(-> configuration-devin-auth-path (configuration) pathname)
(defun configuration-devin-auth-path (configuration)
  "Return Autolith's private Devin credential pathname."
  (merge-pathnames "devin-auth.sexp" (configuration-state-root configuration)))

(-> devin-oauth-authorization-url
    (&key (:redirect-uri string) (:state string) (:code-challenge string))
    string)
(defun devin-oauth-authorization-url (&key redirect-uri state code-challenge)
  "Build the Devin authorization URL for one browser login."
  (format nil "~A?~A"
          *devin-authorize-url*
          (url-encode-params
           (list (cons "response_type" "code")
                 (cons "client_id" "devin-cli")
                 (cons "redirect_uri" redirect-uri)
                 (cons "code_challenge" code-challenge)
                 (cons "code_challenge_method" "S256")
                 (cons "state" state)
                 (cons "prompt" "select_account")))))

(-> devin-oauth--state () string)
(defun devin-oauth--state ()
  "Return a fresh 256-bit OAuth state value."
  (oauth--base64url (random-data 32)))

(-> devin-oauth--open-listener (integer) (option sb-bsd-sockets:inet-socket))
(defun devin-oauth--open-listener (port)
  "Open one IPv4 loopback listener on PORT, or return NIL when unavailable."
  (let ((listener nil))
    (handler-case
        (progn
          (setf listener (make-instance 'sb-bsd-sockets:inet-socket
                                        :type ':stream
                                        :protocol ':tcp))
          (sb-bsd-sockets:socket-bind
           listener
           (sb-bsd-sockets:make-inet-address "127.0.0.1")
           port)
          (sb-bsd-sockets:socket-listen listener 4)
          listener)
      (error ()
        (when listener
          (ignore-errors (sb-bsd-sockets:socket-close listener)))
        nil))))

(-> devin-oauth--write-callback-response (stream string string) null)
(defun devin-oauth--write-callback-response (stream status body)
  "Write one minimal browser response with STATUS and ASCII BODY."
  (format stream
          "HTTP/1.1 ~A~C~CContent-Type: text/plain; charset=utf-8~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
          status
          #\Return #\Linefeed #\Return #\Linefeed
          (length body)
          #\Return #\Linefeed #\Return #\Linefeed
          #\Return #\Linefeed
          body)
  (finish-output stream)
  nil)

(-> devin-oauth--read-request-line (stream integer real) (option string))
(defun devin-oauth--read-request-line (stream file-descriptor deadline)
  "Read one bounded callback request line without exceeding DEADLINE."
  (let* ((started-at (device-authentication-monotonic-seconds))
         (connection-deadline (min deadline (+ started-at *devin-oauth-request-timeout*)))
         (characters (make-array 128
                                 :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
    (loop
      (when (>= (length characters) *devin-oauth-request-line-limit*)
        (return nil))
      (let ((remaining (- connection-deadline (device-authentication-monotonic-seconds))))
        (unless (and (plusp remaining)
                     (or (listen stream)
                         (sb-sys:wait-until-fd-usable file-descriptor ':input remaining)))
          (return nil)))
      (let ((character (read-char stream nil nil)))
        (cond
          ((null character) (return nil))
          ((char= character #\Linefeed)
           (return (string-right-trim '(#\Return) (coerce characters 'string))))
          (t (vector-push-extend character characters)))))))

(-> devin-oauth--request-target (string) (option string))
(defun devin-oauth--request-target (request-line)
  "Return the request target from one HTTP GET request line."
  (when (uiop:string-prefix-p "GET " request-line)
    (let* ((first-space (position #\Space request-line))
           (second-space (and first-space
                              (position #\Space request-line
                                        :start (1+ first-space)))))
      (and second-space
           (subseq request-line (1+ first-space) second-space)))))

(-> devin-oauth--callback-target-p ((option string)) boolean)
(defun devin-oauth--callback-target-p (target)
  "Return true when TARGET addresses the Devin OAuth callback path."
  (and target
       (let ((question (position #\? target)))
         (string= (subseq target 0 question)
                  *devin-oauth-callback-path*))))

(-> devin-oauth--callback-code (string string) string)
(defun devin-oauth--callback-code (target expected-state)
  "Validate one callback TARGET and return its authorization code."
  (let* ((parameters (oauth--query-parameters target))
         (state (rest (assoc "state" parameters :test #'string=)))
         (code (rest (assoc "code" parameters :test #'string=)))
         (raw-error (rest (assoc "error" parameters :test #'string=))))
    (unless (and (stringp state) (string= state expected-state))
      (devin-oauth--fail ':callback "The Devin OAuth callback state did not match."))
    (when raw-error
      (devin-oauth--fail ':authorization
                         (format nil "Devin rejected authorization (~A)." raw-error)
                         :code raw-error))
    (unless (non-empty-string-p code)
      (devin-oauth--fail ':callback "The Devin OAuth callback omitted its authorization code."))
    code))

(-> devin-oauth--callback-code-or-continue (string string) (option string))
(defun devin-oauth--callback-code-or-continue (target expected-state)
  "Return a callback code, or NIL when the callback state is unrelated."
  (let ((state (rest (assoc "state" (oauth--query-parameters target)
                            :test #'string=))))
    (if (and (stringp state) (string= state expected-state))
        (devin-oauth--callback-code target expected-state)
        nil)))

(-> devin-oauth-await-loopback
    (sb-bsd-sockets:inet-socket string &key (:timeout integer))
    string)
(defun devin-oauth-await-loopback (listener expected-state &key (timeout *devin-oauth-callback-timeout*))
  "Wait at most TIMEOUT seconds for a valid Devin browser callback."
  (let ((deadline (+ (device-authentication-monotonic-seconds) timeout)))
    (loop
      (let ((remaining (- deadline (device-authentication-monotonic-seconds))))
        (unless (and (plusp remaining)
                     (sb-sys:wait-until-fd-usable
                      (sb-bsd-sockets:socket-file-descriptor listener)
                      ':input
                      remaining))
          (devin-oauth--fail ':callback-wait
                             "Devin authentication timed out waiting for the browser callback."))
        (let ((socket nil)
              (stream nil))
          (unwind-protect
               (progn
                 (setf socket (sb-bsd-sockets:socket-accept listener)
                       stream (sb-bsd-sockets:socket-make-stream
                               socket
                               :input t
                               :output t
                               :element-type 'character
                               :external-format ':utf-8))
                 (let* ((request-line (devin-oauth--read-request-line
                                       stream
                                       (sb-bsd-sockets:socket-file-descriptor socket)
                                       deadline))
                        (target (and request-line
                                     (devin-oauth--request-target request-line))))
                   (cond
                     ((null request-line)
                      nil)
                     ((not (devin-oauth--callback-target-p target))
                      (devin-oauth--write-callback-response
                       stream "404 Not Found" "Not Found"))
                     (t
                      (handler-case
                          (let ((code (devin-oauth--callback-code-or-continue
                                       target expected-state)))
                            (if code
                                (progn
                                  (devin-oauth--write-callback-response
                                   stream "200 OK"
                                   "Autolith authentication complete. You may close this window.")
                                  (return-from devin-oauth-await-loopback code))
                                (devin-oauth--write-callback-response
                                 stream "400 Bad Request"
                                 "Autolith authentication did not match this login.")))
                        (devin-oauth-error (condition)
                          (devin-oauth--write-callback-response
                           stream "400 Bad Request" "Autolith authentication failed.")
                          (error condition)))))))
            (when stream (ignore-errors (close stream)))
            (when socket (ignore-errors (sb-bsd-sockets:socket-close socket)))))))))

(-> devin-oauth-exchange-token (string string) string)
(defun devin-oauth-exchange-token (code verifier)
  "Exchange authorization CODE and PKCE VERIFIER for a Devin token."
  (handler-case
      (multiple-value-bind (body status)
          (provider-call-with-response-deadline
           30
           (lambda ()
             (dexador:post *devin-token-url*
                           :headers '(("Accept" . "application/json")
                                      ("Content-Type" . "application/json"))
                           :content (json-encode-utf8
                                     (json-object "code" code
                                                  "code_verifier" verifier))
                           :force-string t
                           :keep-alive nil
                           :connect-timeout 30
                           :read-timeout 30)))
        (unless (= status 200)
          (devin-oauth--fail ':token
                             (format nil "Devin token exchange returned HTTP ~D." status)
                             :status status))
        (let* ((response (json-decode body))
               (token (and (json-object-p response) (json-get response "token"))))
          (unless (non-empty-string-p token)
            (devin-oauth--fail ':token "Devin token exchange returned no token."))
          token))
    (devin-oauth-error (condition)
      (error condition))
    (error (cause)
      (devin-oauth--fail ':token
                         (format nil "Devin token exchange failed: ~A" cause)))))

(-> devin-oauth-login
    (&key (:stream stream) (:open-browser-p boolean))
    string)
(defun devin-oauth-login (&key (stream *standard-output*) (open-browser-p t))
  "Run the Devin browser PKCE login and return the stored token.

The token is validated against the Cascade service before it is returned."
  (multiple-value-bind (verifier challenge) (oauth--create-pkce :verifier-octets 64)
    (let* ((state (devin-oauth--state))
           (listener (devin-oauth--open-listener *devin-oauth-callback-port*))
           (redirect-uri (format nil "http://127.0.0.1:~D~A"
                                 *devin-oauth-callback-port*
                                 *devin-oauth-callback-path*)))
      (unless listener
        (devin-oauth--fail
         ':callback-listen
         (format nil "Could not start the Devin OAuth callback server on port ~D."
                 *devin-oauth-callback-port*)))
      (unwind-protect
           (let ((url (devin-oauth-authorization-url
                       :redirect-uri redirect-uri
                       :state state
                       :code-challenge challenge)))
             (format stream "~&Open this URL to sign in to Devin:~%~A~%" url)
             (finish-output stream)
             (when open-browser-p
               (ignore-errors (device-authentication-open-browser url)))
             (let* ((code (devin-oauth-await-loopback listener state))
                    (token (devin-oauth-exchange-token code verifier)))
               (devin-get-user-jwt token)
               token))
        (ignore-errors (sb-bsd-sockets:socket-close listener))))))

(-> devin-credential-manager-create (configuration) credential-manager)
(defun devin-credential-manager-create (configuration)
  "Create the Devin credential manager for CONFIGURATION's private paths."
  (make-instance 'devin-credential-manager
                 :primary-source (make-instance
                                  'autolith-credential-source
                                  :pathname (configuration-devin-auth-path configuration))))

(defclass devin-credential-manager (credential-manager)
  ()
  (:documentation "The Devin OAuth credential manager behind the Devin provider."))

(defmethod credential-manager-provider-label ((manager devin-credential-manager))
  "Name the Devin account service in user-visible failures."
  (declare (ignore manager))
  "Devin")

(defmethod credential-manager-login-hint ((manager devin-credential-manager))
  "Point Devin credential failures at the login command."
  (declare (ignore manager))
  "run autolith auth devin")
