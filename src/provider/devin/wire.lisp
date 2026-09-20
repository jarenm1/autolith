(in-package #:autolith)

;;;; -- Devin (Codeium Cascade) Wire Protocol --

;;; Devin CLI's model backend is Codeium/Windsurf's Cascade service. It speaks
;;; the Connect RPC protocol over HTTP/1.1 with protobuf bodies. This file
;;; builds the requests and parses the responses Autolith needs: GetUserJwt for
;;; authentication, AssignModel for router models, and the streaming
;;; GetChatMessage turn. Field numbers come from the Codeium protobuf schemas
;;; inspected in the oh-my-pi reference checkout.

(defparameter *devin-base-url* "https://server.codeium.com"
  "Base host for Codeium/Windsurf's Cascade API.")

(defparameter *devin-session-token-prefix* "devin-session-token$"
  "The scheme prefix the Cascade wire format requires on a session token.")

(defparameter *devin-auth-path* "/exa.auth_pb.AuthService/GetUserJwt"
  "The unary authentication path exchanging a session token for a user JWT.")

(defparameter *devin-assign-model-path*
  "/exa.api_server_pb.ApiServerService/AssignModel"
  "The unary router path resolving a router model to a concrete model uid.")

(defparameter *devin-chat-path*
  "/exa.api_server_pb.ApiServerService/GetChatMessage"
  "The streaming Cascade chat path.")

(defparameter *devin-cli-model-configs-path*
  "/exa.api_server_pb.ApiServerService/GetCliModelConfigs"
  "The unary path listing the models the Devin CLI may select.")

(defparameter *devin-ide-name* "devin-cli"
  "The released Devin CLI identity name the backend gates behavior on.")

(defparameter *devin-ide-type* "chisel"
  "The identity type that unlocks router assignment and the CLI model surface.")

(defparameter *devin-ide-version* "3000.6.2"
  "The released Devin CLI version announced on every call.")

(defparameter *devin-extension-name* "chisel"
  "The extension name announced alongside the CLI identity.")

(defparameter *devin-extension-version* "3000.6.2"
  "The extension version announced alongside the CLI identity.")

(defparameter *devin-default-stop-patterns*
  '("<|user|>" "<|bot|>" "<|context_request|>" "<|endoftext|>" "<|end_of_turn|>")
  "The stop patterns the Cascade backend expects on every chat request.")

(defparameter *devin-connect-compressed-flag* #x01
  "Connect frame flag bit marking a gzip-compressed payload.")

(defparameter *devin-connect-end-stream-flag* #x02
  "Connect frame flag bit marking an end-of-stream JSON trailer.")

(defparameter *devin-maximum-frame-payload* (* 16 1024 1024)
  "The hard upper bound on one Connect frame payload in octets.")

(defparameter *devin-chat-message-source-user* 1
  "ChatMessageSource value for a user turn.")

(defparameter *devin-chat-message-source-system* 2
  "ChatMessageSource value for an assistant turn.")

(defparameter *devin-chat-message-source-tool* 4
  "ChatMessageSource value for a tool result.")

(defparameter *devin-chat-message-request-type-cascade* 5
  "ChatMessageRequestType value for a Cascade turn.")

(defparameter *devin-planner-mode-default* 1
  "ConversationalPlannerMode value for an ordinary turn.")

(defparameter *devin-cache-control-ephemeral* 1
  "CacheControlType value requesting an ephemeral prompt cache.")

(defparameter *devin-stop-reason-max-tokens* 3
  "StopReason value reporting the model reached its output limit.")

(define-condition devin-error (provider-error)
  ((operation
    :initarg :operation
    :reader devin-error-operation
    :type keyword
    :documentation "The authentication, assignment, or chat operation that failed.")
   (status
    :initarg :status
    :initform nil
    :reader devin-error-status
    :type (option integer)
    :documentation "The HTTP status returned by the Cascade service, if known.")
   (detail
    :initarg :detail
    :initform nil
    :reader devin-error-detail
    :type (option string)
    :documentation "A bounded, non-secret server error detail, if supplied."))
  (:documentation "A Devin Cascade protocol operation failed."))

(-> devin--fail
    (keyword string &key (:status (option integer)) (:detail (option string)))
    nil)
(defun devin--fail (operation message &key status detail)
  "Signal a structured Devin protocol failure."
  (error 'devin-error
         :message message
         :operation operation
         :status status
         :detail detail))

(-> devin--os-name () string)
(defun devin--os-name ()
  "Return the Metadata OS vocabulary for the current host."
  (ecase (platform-host-name *platform*)
    (:linux "linux")
    (:macos "darwin")
    (:windows "windows")
    (:bsd "linux")
    (:posix "linux")))

(-> devin--session-token (string) string)
(defun devin--session-token (token)
  "Return TOKEN with the required session-token scheme prefix."
  (if (uiop:string-prefix-p *devin-session-token-prefix* token)
      token
      (concatenate 'string *devin-session-token-prefix* token)))

(-> devin--timestamp-octets () (vector (unsigned-byte 8)))
(defun devin--timestamp-octets ()
  "Return the encoded Timestamp message for the current time."
  (let ((buffer (protobuf--writer)))
    (protobuf-write-uint64 buffer 1 (- (get-universal-time) 2208988800))
    buffer))

(-> devin--metadata-octets
    (string &key (:user-jwt string) (:session-id string)
            (:request-id integer) (:trigger-id string))
    (vector (unsigned-byte 8)))
(defun devin--metadata-octets
    (token &key (user-jwt "") (session-id "") (request-id 0) (trigger-id ""))
  "Return the encoded Metadata message for a Cascade call.

TOKEN is the session token; USER-JWT is empty for the calls the CLI makes with
the session token alone."
  (let ((buffer (protobuf--writer)))
    (protobuf-write-string buffer 1 *devin-ide-name*)
    (protobuf-write-string buffer 2 *devin-extension-version*)
    (protobuf-write-string buffer 3 (devin--session-token token))
    (protobuf-write-string buffer 4 "en")
    (protobuf-write-string buffer 5 (devin--os-name))
    (protobuf-write-string buffer 7 *devin-ide-version*)
    (protobuf-write-uint64 buffer 9 request-id)
    (protobuf-write-string buffer 10 session-id)
    (protobuf-write-string buffer 12 *devin-ide-name*)
    (protobuf-write-message buffer 16 (devin--timestamp-octets))
    (when (plusp (length user-jwt))
      (protobuf-write-string buffer 21 user-jwt))
    (protobuf-write-string buffer 25 trigger-id)
    (protobuf-write-string buffer 26 "Unset")
    (protobuf-write-string buffer 28 *devin-ide-type*)
    buffer))


;;;; -- Connect Framing --

(-> devin--connect-frame ((vector (unsigned-byte 8))) (vector (unsigned-byte 8)))
(defun devin--connect-frame (payload)
  "Return PAYLOAD wrapped in one uncompressed Connect frame."
  (let ((frame (make-array (+ 5 (length payload))
                           :element-type '(unsigned-byte 8))))
    (setf (aref frame 0) 0)
    (loop for shift from 24 downto 0 by 8
          for index from 1
          do (setf (aref frame index)
                   (ldb (byte 8 shift) (length payload))))
    (replace frame payload :start1 5)
    frame))

(-> devin--connect-unary
    (string (vector (unsigned-byte 8)) &key (:timeout integer))
    (vector (unsigned-byte 8)))
(defun devin--connect-unary (path request &key (timeout 30))
  "POST REQUEST to PATH as a Connect unary call and return the response message.

Unary Connect calls carry the raw protobuf message with the application/proto
content type; the five-octet envelope applies only to streaming calls."
  (declare (ignore token))
  (handler-case
      (multiple-value-bind (payload status headers)
          (provider-call-with-response-deadline
           timeout
           (lambda ()
             (dexador:post (concatenate 'string *devin-base-url* path)
                           :headers '(("content-type" . "application/proto")
                                      ("accept-encoding" . "identity"))
                           :content request
                           :force-string nil
                           :keep-alive nil
                           :connect-timeout timeout
                           :read-timeout timeout)))
        (declare (ignore headers))
        (unless (= status 200)
          (devin--fail ':request
                       (format nil "Devin ~A returned HTTP ~D." path status)
                       :status status))
        payload)
    (devin-error (condition)
      (error condition))
    (error (cause)
      (devin--fail ':request
                   (format nil "Devin ~A request failed: ~A" path cause)))))


;;;; -- Authentication --

(-> devin-get-user-jwt (string &key (:base-url string)) string)
(defun devin-get-user-jwt (token &key (base-url *devin-base-url*))
  "Exchange session TOKEN for a Cascade user JWT."
  (let* ((request (let ((buffer (protobuf--writer)))
                    (protobuf-write-message buffer 1 (devin--metadata-octets token))
                    buffer))
         (payload (devin--connect-unary-at base-url *devin-auth-path* request))
         (reader (protobuf-reader-create payload))
         (user-jwt nil))
    (loop until (protobuf-reader-exhausted-p reader)
          do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
               (if (and (= field 1) (= wire-type +protobuf-wire-length-delimited+))
                   (setf user-jwt (protobuf-read-string reader))
                   (protobuf-skip-field reader wire-type))))
    (unless (non-empty-string-p user-jwt)
      (devin--fail ':authentication "Devin returned an empty user JWT."))
    user-jwt))

(-> devin--connect-unary-at
    (string string (vector (unsigned-byte 8)))
    (vector (unsigned-byte 8)))
(defun devin--connect-unary-at (base-url path request)
  "POST REQUEST to BASE-URL plus PATH as a Connect unary call."
  (let ((*devin-base-url* base-url))
    (devin--connect-unary path request)))


;;;; -- Model Assignment --

(-> devin-assign-model (string string string &key (:base-url string)) (values string string))
(defun devin-assign-model (token user-jwt router-uid &key (base-url *devin-base-url*))
  "Resolve ROUTER-UID to a concrete model uid and its assignment JWT."
  (let* ((request (let ((buffer (protobuf--writer)))
                    (protobuf-write-message buffer 1 (devin--metadata-octets token))
                    (protobuf-write-string buffer 2 router-uid)
                    (protobuf-write-string buffer 3 (make-identifier))
                    buffer))
         (payload (devin--connect-unary-at base-url *devin-assign-model-path* request))
         (reader (protobuf-reader-create payload))
         (model-uid nil)
         (assignment-jwt nil))
    (loop until (protobuf-reader-exhausted-p reader)
          do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
               (if (and (= field 1) (= wire-type +protobuf-wire-length-delimited+))
                   (let ((nested (protobuf-reader-create
                                  (protobuf-read-length-delimited reader))))
                     (loop until (protobuf-reader-exhausted-p nested)
                           do (multiple-value-bind (inner-field inner-wire)
                                  (protobuf-read-tag nested)
                                (cond
                                  ((and (= inner-field 1)
                                        (= inner-wire +protobuf-wire-length-delimited+))
                                   (setf assignment-jwt (protobuf-read-string nested)))
                                  ((and (= inner-field 2)
                                        (= inner-wire +protobuf-wire-length-delimited+))
                                   (setf model-uid (protobuf-read-string nested)))
                                  (t (protobuf-skip-field nested inner-wire))))))
                   (protobuf-skip-field reader wire-type))))
    (unless (and (non-empty-string-p model-uid)
                 (non-empty-string-p assignment-jwt))
      (devin--fail ':assignment
                   "Devin AssignModel returned no model uid and assignment JWT."))
    (values model-uid assignment-jwt)))
