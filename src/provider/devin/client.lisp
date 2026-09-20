(in-package #:autolith)

;;;; -- Devin Provider --

;;; The Devin provider speaks Codeium/Windsurf's Cascade Connect RPC protocol.
;;; It authenticates with a Devin session token, exchanges it for a user JWT,
;;; optionally resolves a router model, and streams one GetChatMessage turn.

(defparameter *devin-models*
  '((:name "devin/adaptive"
     :description "Devin Adaptive router, which selects a model per task."
     :context-window 200000)
    (:name "devin/swe-1.7"
     :description "Devin's fast software-engineering model."
     :context-window 262000)
    (:name "devin/claude-opus-5"
     :description "Claude Opus through Devin."
     :context-window 1000000)
    (:name "devin/claude-fable-5"
     :description "Claude Fable through Devin."
     :context-window 1000000)
    (:name "devin/gpt-5.6-sol"
     :description "GPT-5.6 Sol through Devin."
     :context-window 1000000))
  "The Devin models offered when the CLI model list is unavailable.")

(defparameter *devin-router-models* '("devin/adaptive")
  "Model identifiers the Cascade backend resolves through AssignModel.")

(defparameter *devin-model-uids*
  '(("devin/swe-1.7" . "swe-1-7")
    ("devin/claude-opus-5" . "claude-opus-5-low")
    ("devin/claude-fable-5" . "claude-5-fable-low")
    ("devin/gpt-5.6-sol" . "gpt-5-6-sol-low"))
  "Wire model uids for the friendly Devin model names.")

(defclass devin-provider
    (session-preserving-provider-mixin subscription-provider)
  ((endpoint
    :initarg :endpoint
    :initform *devin-base-url*
    :reader devin-provider-endpoint
    :type non-empty-string
    :documentation "The Cascade base URL.")
   (user-jwt
    :initarg :user-jwt
    :initform nil
    :accessor devin-provider-user-jwt
    :type (option string)
    :documentation "The cached Cascade user JWT for the current session token."))
  (:documentation "A direct Devin CLI subscription provider for Cascade."))

(-> devin-provider-create
    (configuration &key (:credential-manager (option credential-manager)))
    devin-provider)
(defun devin-provider-create (configuration &key credential-manager)
  "Create the Devin provider for CONFIGURATION."
  (make-instance 'devin-provider
                 :configuration configuration
                 :credential-manager
                 (or credential-manager (devin-credential-manager-create configuration))
                 :session-id (make-identifier)))

(-> devin-provider--user-jwt (devin-provider oauth-credentials) string)
(defun devin-provider--user-jwt (provider credentials)
  "Return PROVIDER's Cascade user JWT for CREDENTIALS, caching it per token."
  (let ((token (oauth-credentials-access-token credentials)))
    (or (devin-provider-user-jwt provider)
        (setf (devin-provider-user-jwt provider)
              (devin-get-user-jwt token :base-url (devin-provider-endpoint provider))))))

(-> devin-provider--model-uid (devin-provider string) (values string (option string)))
(defun devin-provider--model-uid (provider model)
  "Return the wire model uid and optional assignment JWT for MODEL."
  (if (member model *devin-router-models* :test #'string=)
      (let ((credentials (credential-manager-credentials
                          (provider-credential-manager provider))))
        (devin-assign-model (oauth-credentials-access-token credentials)
                            (devin-provider--user-jwt provider credentials)
                            (subseq model (length "devin/"))
                            :base-url (devin-provider-endpoint provider)))
      (values (or (cdr (assoc model *devin-model-uids* :test #'string=))
                  model)
              nil)))

(-> devin--prompt-source (string) integer)
(defun devin--prompt-source (role)
  "Return the Cascade ChatMessageSource value for ROLE."
  (cond
    ((string= role "user") *devin-chat-message-source-user*)
    ((string= role "assistant") *devin-chat-message-source-system*)
    (t *devin-chat-message-source-tool*)))

(-> devin--prompt-octets (string integer string &key (:tool-call-id (option string))) (vector (unsigned-byte 8)))
(defun devin--prompt-octets (message-id source prompt &key tool-call-id)
  "Return one encoded ChatMessagePrompt."
  (let ((buffer (protobuf--writer)))
    (protobuf-write-string buffer 1 message-id)
    (protobuf-write-uint64 buffer 2 source)
    (protobuf-write-string buffer 3 prompt)
    (when tool-call-id
      (protobuf-write-string buffer 7 tool-call-id))
    buffer))

(-> devin--tool-octets (string string json-object) (vector (unsigned-byte 8)))
(defun devin--tool-octets (name description schema)
  "Return one encoded ChatToolDefinition."
  (let ((buffer (protobuf--writer)))
    (protobuf-write-string buffer 1 name)
    (protobuf-write-string buffer 2 description)
    (protobuf-write-string buffer 3 (json-encode schema))
    buffer))

(-> devin--chat-request
    (devin-provider conversation vector
     &key (:model-uid string) (:user-jwt string) (:token string))
    (vector (unsigned-byte 8)))
(defun devin--chat-request (provider conversation tool-namespaces
                            &key model-uid user-jwt token)
  "Return the encoded GetChatMessageRequest for one Cascade turn."
  (let* ((configuration (provider-configuration provider))
         (cascade-id (conversation-identifier conversation))
         (items (conversation-input-items-for-family
                 conversation (provider-family provider)))
         (tools (provider-request-tool-namespaces configuration tool-namespaces))
         (buffer (protobuf--writer)))
    (protobuf-write-message buffer 1
                            (devin--metadata-octets
                             token
                             :user-jwt user-jwt
                             :session-id (provider-session-id provider)
                             :request-id (- (get-universal-time) 2208988800)
                             :trigger-id (make-identifier)))
    (protobuf-write-string buffer 2 (system-prompt configuration))
    (loop for item in items
          for index from 0
          for role = (json-get item "role")
          for text = (devin--item-text item)
          when (and (stringp role) (non-empty-string-p text))
            do (protobuf-write-message
                buffer 3
                (devin--prompt-octets
                 (format nil "~A-~D" cascade-id index)
                 (devin--prompt-source role)
                 text)))
    (protobuf-write-uint64 buffer 7 *devin-chat-message-request-type-cascade*)
    (protobuf-write-message buffer 8 (devin--completion-configuration provider))
    (loop for namespace across tools
          for namespace-name = (and (json-object-p namespace)
                                    (json-get namespace "name"))
          do (loop for tool across (or (and (json-object-p namespace)
                                            (json-get namespace "tools"))
                                       #())
                   for name = (and (json-object-p tool) (json-get tool "name"))
                   when (and (non-empty-string-p namespace-name)
                             (non-empty-string-p name))
                     do (protobuf-write-message
                         buffer 10
                         (devin--tool-octets
                          name
                          (or (json-get tool "description") "")
                          (or (json-get tool "parameters")
                              (json-object))))))
    (protobuf-write-message buffer 15 (devin--trajectory-reference
                                       (make-identifier)))
    (protobuf-write-string buffer 16 cascade-id)
    (protobuf-write-uint64 buffer 20 *devin-planner-mode-default*)
    (protobuf-write-string buffer 21 model-uid)
    (protobuf-write-message buffer 13
                            (let ((cache (protobuf--writer)))
                              (protobuf-write-uint64 cache 1 *devin-cache-control-ephemeral*)
                              cache))
    buffer))

(-> devin--item-text (json-object) string)
(defun devin--item-text (item)
  "Return ITEM's concatenated text content."
  (let ((content (json-get item "content")))
    (cond
      ((stringp content) content)
      ((vectorp content)
       (with-output-to-string (stream)
         (loop for part across content
               when (and (json-object-p part)
                         (stringp (json-get part "text")))
                 do (write-string (json-get part "text") stream))))
      (t ""))))

(-> devin--trajectory-reference (string) (vector (unsigned-byte 8)))
(defun devin--trajectory-reference (trajectory-id)
  "Return the encoded CortexTrajectoryReference for TRAJECTORY-ID."
  (let ((buffer (protobuf--writer)))
    (protobuf-write-string buffer 1 trajectory-id)
    (protobuf-write-uint64 buffer 3 4)
    (protobuf-write-uint64 buffer 4 14)
    buffer))

(-> devin--completion-configuration (devin-provider) (vector (unsigned-byte 8)))
(defun devin--completion-configuration (provider)
  "Return the encoded CompletionConfiguration for PROVIDER's model."
  (declare (ignore provider))
  (let ((buffer (protobuf--writer)))
    (protobuf-write-uint64 buffer 1 1)
    (protobuf-write-uint64 buffer 2 128000)
    (protobuf-write-uint64 buffer 3 400)
    (protobuf-write-double buffer 5 1.0d0)
    (protobuf-write-uint64 buffer 7 40)
    (protobuf-write-double buffer 8 0.95d0)
    buffer))

(-> devin--parse-chat-response
    (devin-provider (vector (unsigned-byte 8)) function stream stream hash-table)
    (values string (option integer)))
(defun devin--parse-chat-response (provider payload event-callback text thinking
                                   tool-calls)
  "Parse one GetChatMessageResponse PAYLOAD, emitting deltas into TEXT and THINKING
and accumulating tool-call argument deltas in TOOL-CALLS keyed by call id."
  (declare (ignore provider))
  (let ((reader (protobuf-reader-create payload))
        (response-id "")
        (stop-reason nil))
    (loop until (protobuf-reader-exhausted-p reader)
          do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
               (cond
                 ((and (= field 1) (= wire-type +protobuf-wire-length-delimited+))
                  (setf response-id (protobuf-read-string reader)))
                 ((and (= field 3) (= wire-type +protobuf-wire-length-delimited+))
                  (let ((delta (protobuf-read-string reader)))
                    (write-string delta text)
                    (funcall event-callback
                             (make-instance 'assistant-delta-event :text delta))))
                 ((and (= field 9) (= wire-type +protobuf-wire-length-delimited+))
                  (let ((delta (protobuf-read-string reader)))
                    (write-string delta thinking)
                    (funcall event-callback
                             (make-instance 'reasoning-delta-event :text delta))))
                 ((and (= field 6) (= wire-type +protobuf-wire-length-delimited+))
                  (devin--parse-tool-call reader tool-calls))
                 ((and (= field 5) (= wire-type +protobuf-wire-varint+))
                  (setf stop-reason (protobuf-read-varint reader)))
                 (t (protobuf-skip-field reader wire-type)))))
    (values response-id stop-reason)))

(-> devin--parse-tool-call (protobuf-reader hash-table) null)
(defun devin--parse-tool-call (reader tool-calls)
  "Read one nested ChatToolCall from READER into TOOL-CALLS keyed by call id.

TOOL-CALLS maps call ids to (name args-stream) plists and carries a
:current-id key for argument deltas that arrive without an id."
  (let* ((octets (protobuf-read-length-delimited reader))
         (inner (protobuf-reader-create octets))
         (id nil)
         (name nil)
         (args-delta nil))
    (loop until (protobuf-reader-exhausted-p inner)
          do (multiple-value-bind (field wire-type) (protobuf-read-tag inner)
               (if (= wire-type +protobuf-wire-length-delimited+)
                   (let ((text (protobuf-read-string inner)))
                     (case field
                       (1 (setf id text))
                       (2 (setf name text))
                       (3 (setf args-delta text))))
                   (protobuf-skip-field inner wire-type))))
    (when id
      (setf (gethash :current-id tool-calls) id))
    (let ((call-id (or id (gethash :current-id tool-calls))))
      (when (and call-id name)
        (let ((entry (or (gethash call-id tool-calls)
                         (setf (gethash call-id tool-calls)
                               (list :name name
                                     :args (make-string-output-stream))))))
          (setf (getf entry :name) name)))
      (when (and call-id args-delta)
        (let ((entry (gethash call-id tool-calls)))
          (when entry
            (write-string args-delta (getf entry :args))))))
    nil))

(-> devin--tool-name-map (devin-provider vector) hash-table)
(defun devin--tool-name-map (provider tool-namespaces)
  "Return a flat-name -> (namespace name) map for PROVIDER's request tools."
  (let ((map (make-hash-table :test 'equal))
        (tools (provider-request-tool-namespaces
                (provider-configuration provider) tool-namespaces)))
    (loop for namespace across tools
          for namespace-name = (and (json-object-p namespace)
                                    (json-get namespace "name"))
          do (loop for tool across (or (and (json-object-p namespace)
                                            (json-get namespace "tools"))
                                       #())
                   for name = (and (json-object-p tool) (json-get tool "name"))
                   when (and (non-empty-string-p namespace-name)
                             (non-empty-string-p name))
                     do (setf (gethash name map) (list namespace-name name))))
    map))

(-> devin--stream-turn
    (devin-provider conversation vector function)
    provider-result)
(defun devin--stream-turn (provider conversation tool-namespaces event-callback)
  "Stream one Cascade turn for CONVERSATION and return the provider result."
  (with-credentials (credentials (provider-credential-manager provider))
    (let* ((token (oauth-credentials-access-token credentials))
           (user-jwt (devin-provider--user-jwt provider credentials))
           (model (configuration-model (provider-configuration provider))))
      (multiple-value-bind (model-uid assignment-jwt)
          (devin-provider--model-uid provider model)
        (declare (ignore assignment-jwt))
        (let* ((request (devin--chat-request provider conversation tool-namespaces
                                             :model-uid model-uid
                                             :user-jwt user-jwt
                                             :token token))
               (body (devin--connect-frame request))
               (tool-name-map (devin--tool-name-map provider tool-namespaces))
               (response-id "")
               (text (make-string-output-stream))
               (thinking (make-string-output-stream))
               (tool-calls (make-hash-table :test 'equal))
               (stop-reason nil))
          (handler-case
              (multiple-value-bind (stream status)
                  (provider-call-with-response-deadline
                   300
                   (lambda ()
                     (dexador:post (concatenate 'string
                                                (devin-provider-endpoint provider)
                                                *devin-chat-path*)
                                   :headers '(("content-type" . "application/connect+proto")
                                              ("connect-protocol-version" . "1")
                                              ("accept-encoding" . "identity"))
                                   :content body
                                   :want-stream t
                                   :force-string nil
                                   :keep-alive nil
                                   :connect-timeout 30
                                   :read-timeout 300)))
                (unless (= status 200)
                  (devin--fail ':chat
                               (format nil "Devin chat returned HTTP ~D." status)
                               :status status))
                (loop for payload = (devin--read-stream-frame stream)
                      while payload
                      do (multiple-value-bind (id reason)
                             (devin--parse-chat-response provider payload
                                                         event-callback text thinking
                                                         tool-calls)
                           (when (non-empty-string-p id)
                             (setf response-id id))
                           (when reason
                             (setf stop-reason reason))))
                (close stream))
            (devin-error (condition) (error condition))
            (error (cause)
              (devin--fail ':chat (format nil "Devin chat failed: ~A" cause))))
          (let ((reasoning (get-output-stream-string thinking))
                (message (get-output-stream-string text))
                (items nil)
                (calls nil))
            (when (plusp (length reasoning))
              (push (json-object "type" "reasoning_content" "content" reasoning) items))
            (when (plusp (length message))
              (push (json-object "type" "message" "role" "assistant"
                                 "content" (json-array (json-object "type" "output_text"
                                                                    "text" message)))
                    items))
            (setf items (nreverse items))
            (maphash (lambda (id entry)
                       (unless (eq id :current-id)
                         (let* ((wire-name (getf entry :name))
                                (arguments (get-output-stream-string (getf entry :args)))
                                (item (json-object "type" "function_call"
                                                   "call_id" id
                                                   "name" wire-name
                                                   "arguments" arguments)))
                           (let ((resolved (gethash wire-name tool-name-map)))
                             (when resolved
                               (setf (gethash "namespace" item) (first resolved)
                                     (gethash "name" item) (second resolved))))
                           (push item items)
                           (push item calls))))
                     tool-calls)
            (setf items (nreverse items)
                  calls (nreverse calls))
            (dolist (item items)
              (funcall event-callback (make-instance 'provider-item-event :item item)))
            (let ((turn-completion (if calls ':continue ':end)))
              (funcall event-callback
                       (make-instance 'provider-completed-event
                                      :response-id response-id
                                      :usage nil
                                      :turn-completion turn-completion))
              (make-instance 'provider-result
                             :response-id response-id
                             :output-items items
                             :tool-calls calls
                             :usage nil
                             :turn-state nil
                             :turn-completion turn-completion))))))))

(-> devin--read-stream-frame (stream) (option (vector (unsigned-byte 8))))
(defun devin--read-stream-frame (stream)
  "Read one Connect data frame from STREAM, or NIL at end of stream."
  (let ((header (make-array 5 :element-type '(unsigned-byte 8))))
    (unless (= 5 (read-sequence header stream))
      (return-from devin--read-stream-frame nil))
    (let ((flag (aref header 0))
          (length (logior (ash (aref header 1) 24)
                          (ash (aref header 2) 16)
                          (ash (aref header 3) 8)
                          (aref header 4))))
      (when (> length *devin-maximum-frame-payload*)
        (devin--fail ':protocol "A Connect frame declared an oversized payload."))
      (let ((payload (make-array length :element-type '(unsigned-byte 8))))
        (unless (= length (read-sequence payload stream))
          (devin--fail ':protocol "A Connect frame ended mid-payload."))
        (when (logtest flag *devin-connect-end-stream-flag*)
          (return-from devin--read-stream-frame nil))
        (when (logtest flag *devin-connect-compressed-flag*)
          (devin--fail ':protocol "A Connect frame was gzip-compressed, which Autolith does not request."))
        payload))))

(defmethod provider-attempt-turn
    ((provider devin-provider) (conversation conversation)
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Stream one Devin Cascade turn."
  (declare (ignore force-refresh goal-context compaction-p))
  (devin--stream-turn provider conversation tool-namespaces event-callback))

(-> devin-model-discovery (configuration) list)
(defun devin-model-discovery (configuration)
  "Return the Devin models for CONFIGURATION, falling back to the static list."
  (declare (ignore configuration))
  (mapcar (lambda (spec) (getf spec :name)) *devin-models*))

(-> devin-authenticate (devin-provider &key (:stream stream) (:open-browser-p boolean)) string)
(defun devin-authenticate (provider &key (stream *standard-output*) (open-browser-p t))
  "Run the Devin browser login and store the resulting credential."
  (let* ((manager (provider-credential-manager provider))
         (token (devin-oauth-login :stream stream :open-browser-p open-browser-p))
         (credentials (make-instance 'oauth-credentials
                                     :access-token token
                                     :refresh-token token
                                     :account-id "devin"
                                     :expires-at (+ (get-universal-time)
                                                    (* 365 24 60 60)))))
    (credential-manager-accept-account manager credentials :allow-change t)
    (credential-source-save (credential-manager-primary-source manager)
                            credentials)
    "Devin authentication was saved by Autolith."))

(register-provider
 "devin"
 :description "Devin CLI subscription (Codeium Cascade)"
 :family ':devin
 :models *devin-models*
 :factory (lambda (configuration &key reasoning-summaries-p)
            (declare (ignore reasoning-summaries-p))
            (devin-provider-create configuration))
 :authenticator #'devin-authenticate
 :protocol ':connect
 :endpoint *devin-base-url*
 :model-discovery #'devin-model-discovery
 :source ':builtin)
