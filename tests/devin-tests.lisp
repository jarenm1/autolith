(in-package #:autolith)

;;;; -- Devin Provider Tests --

(-> test-devin-protobuf-roundtrip () boolean)
(defun test-devin-protobuf-roundtrip ()
  "The protobuf codec round-trips the scalar and nested field types Devin uses."
  (let* ((buffer (protobuf--writer))
         (nested (let ((inner (protobuf--writer)))
                   (protobuf-write-string inner 1 "hello")
                   (protobuf-write-uint64 inner 2 300)
                   inner)))
    (protobuf-write-string buffer 1 "devin")
    (protobuf-write-uint64 buffer 2 150)
    (protobuf-write-bool buffer 3 t)
    (protobuf-write-double buffer 4 0.5d0)
    (protobuf-write-message buffer 5 nested)
    (let ((reader (protobuf-reader-create buffer))
          (string-value nil)
          (uint-value nil)
          (bool-value nil)
          (double-value nil)
          (nested-string nil)
          (nested-uint nil))
      (loop until (protobuf-reader-exhausted-p reader)
            do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
                 (cond
                   ((and (= field 1) (= wire-type +protobuf-wire-length-delimited+))
                    (setf string-value (protobuf-read-string reader)))
                   ((and (= field 2) (= wire-type +protobuf-wire-varint+))
                    (setf uint-value (protobuf-read-varint reader)))
                   ((and (= field 3) (= wire-type +protobuf-wire-varint+))
                    (setf bool-value (protobuf-read-bool reader)))
                   ((and (= field 4) (= wire-type +protobuf-wire-fixed64+))
                    (setf double-value (protobuf-read-double reader)))
                   ((and (= field 5) (= wire-type +protobuf-wire-length-delimited+))
                    (let ((inner (protobuf-reader-create
                                  (protobuf-read-length-delimited reader))))
                      (loop until (protobuf-reader-exhausted-p inner)
                            do (multiple-value-bind (inner-field inner-wire)
                                   (protobuf-read-tag inner)
                                 (cond
                                   ((and (= inner-field 1)
                                         (= inner-wire +protobuf-wire-length-delimited+))
                                    (setf nested-string (protobuf-read-string inner)))
                                   ((and (= inner-field 2)
                                         (= inner-wire +protobuf-wire-varint+))
                                    (setf nested-uint (protobuf-read-varint inner)))
                                   (t (protobuf-skip-field inner inner-wire)))))))
                   (t (protobuf-skip-field reader wire-type)))))
      (test-assert (string= string-value "devin") "a string field round-trips")
      (test-assert (= uint-value 150) "a varint field round-trips")
      (test-assert (eq bool-value t) "a boolean field round-trips")
      (test-assert (= double-value 0.5d0) "a double field round-trips")
      (test-assert (string= nested-string "hello") "a nested string round-trips")
      (test-assert (= nested-uint 300) "a nested varint round-trips"))))

(-> test-devin-connect-frame () boolean)
(defun test-devin-connect-frame ()
  "A Connect frame carries the payload length and an uncompressed flag."
  (let* ((payload (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3)))
         (frame (devin--connect-frame payload)))
    (test-assert (= (length frame) 8) "the frame has a five-octet header")
    (test-assert (zerop (aref frame 0)) "the frame is uncompressed")
    (test-assert (= (aref frame 4) 3) "the frame records the payload length")
    (test-assert (equalp (subseq frame 5) payload) "the frame carries the payload")))

(-> test-devin-metadata () boolean)
(defun test-devin-metadata ()
  "Devin metadata carries the session-token scheme prefix and CLI identity."
  (let* ((octets (devin--metadata-octets "abc"))
         (reader (protobuf-reader-create octets))
         (api-key nil)
         (ide-type nil))
    (loop until (protobuf-reader-exhausted-p reader)
          do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
               (cond
                 ((and (= field 3) (= wire-type +protobuf-wire-length-delimited+))
                  (setf api-key (protobuf-read-string reader)))
                 ((and (= field 28) (= wire-type +protobuf-wire-length-delimited+))
                  (setf ide-type (protobuf-read-string reader)))
                 (t (protobuf-skip-field reader wire-type)))))
    (test-assert (string= api-key "devin-session-token$abc")
                 "the session token carries its required prefix")
    (test-assert (string= ide-type "chisel")
                 "the metadata announces the CLI identity")))

(-> test-devin-authorization-url () boolean)
(defun test-devin-authorization-url ()
  "The authorization URL carries the PKCE challenge, state, and redirect."
  (let ((url (devin-oauth-authorization-url
              :redirect-uri "http://127.0.0.1:59653/callback"
              :state "state-123"
              :code-challenge "challenge-456")))
    (test-assert (uiop:string-prefix-p "https://app.devin.ai/auth/cli/continue?" url)
                 "the URL targets the Devin CLI authorization endpoint")
    (test-assert (search "code_challenge=challenge-456" url)
                 "the URL carries the PKCE challenge")
    (test-assert (search "state=state-123" url) "the URL carries the state")
    (test-assert (search "code_challenge_method=S256" url)
                 "the URL requests the S256 challenge method")))

(-> test-devin-callback-code () boolean)
(defun test-devin-callback-code ()
  "The callback parser validates state and returns the authorization code."
  (test-assert (string= (devin-oauth--callback-code
                         "/callback?code=abc&state=xyz" "xyz")
                        "abc")
               "a matching callback yields its code")
  (test-assert (handler-case
                   (progn (devin-oauth--callback-code
                           "/callback?code=abc&state=other" "xyz")
                          nil)
                 (devin-oauth-error () t))
               "a mismatched state is rejected")
  (test-assert (handler-case
                   (progn (devin-oauth--callback-code "/callback?state=xyz" "xyz")
                          nil)
                 (devin-oauth-error () t))
               "a callback without a code is rejected"))

(-> test-devin-provider-registration () boolean)
(defun test-devin-provider-registration ()
  "The Devin provider is registered with its family and models."
  (let ((registration (provider-registration-find "devin")))
    (test-assert (not (null registration)) "the Devin provider is registered")
    (test-assert (eq (provider-registration-family registration) ':devin)
                 "the Devin family is registered")
    (test-assert (member "devin/adaptive"
                         (mapcar #'provider-model-name
                                 (provider-registration-models registration))
                         :test #'string=)
                 "the Adaptive router model is offered")
    (test-assert (not (null (provider-registration-authenticator registration)))
                 "the Devin provider exposes an authenticator")))

(-> test-devin-user-jwt-request () boolean)
(defun test-devin-user-jwt-request ()
  "The GetUserJwt request nests the metadata message in field one."
  (let* ((metadata (devin--metadata-octets "token"))
         (request (let ((buffer (protobuf--writer)))
                    (protobuf-write-message buffer 1 metadata)
                    buffer))
         (reader (protobuf-reader-create request)))
    (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
      (test-assert (and (= field 1) (= wire-type +protobuf-wire-length-delimited+))
                   "the metadata is field one of the request")
      (test-assert (equalp (protobuf-read-length-delimited reader) metadata)
                   "the nested metadata round-trips"))))

(-> devin-test--provider () devin-provider)
(defun devin-test--provider ()
  "Return an isolated Devin provider with a test credential manager."
  (let ((configuration (test-configuration)))
    (devin-provider-create
     configuration
     :credential-manager (devin-credential-manager-create configuration))))

(-> devin-test--frame-stream ((vector (unsigned-byte 8))) stream)
(defun devin-test--frame-stream (octets)
  "Return a binary input stream over OCTETS."
  (flexi-streams:make-in-memory-input-stream octets))

(-> test-devin-read-stream-frame () boolean)
(defun test-devin-read-stream-frame ()
  "The Connect frame reader parses big-endian lengths and end-stream flags."
  (let* ((payload (make-array 3 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3)))
         (frame (devin--connect-frame payload))
         (stream (devin-test--frame-stream frame)))
    (test-assert (equalp (devin--read-stream-frame stream) payload)
                 "a data frame round-trips")
    (test-assert (null (devin--read-stream-frame stream))
                 "end of stream returns nil"))
  (let* ((end-frame (make-array 5 :element-type '(unsigned-byte 8)
                                  :initial-contents '(2 0 0 0 0)))
         (stream (devin-test--frame-stream end-frame)))
    (test-assert (null (devin--read-stream-frame stream))
                 "an end-stream frame returns nil"))
  (let* ((oversized (make-array 5 :element-type '(unsigned-byte 8)
                                :initial-contents '(0 #xFF #xFF #xFF #xFF)))
         (stream (devin-test--frame-stream oversized)))
    (test-assert (handler-case
                     (progn (devin--read-stream-frame stream) nil)
                   (devin-error () t))
                 "an oversized frame is rejected"))
  (let* ((truncated (make-array 6 :element-type '(unsigned-byte 8)
                                :initial-contents '(0 0 0 0 5 1)))
         (stream (devin-test--frame-stream truncated)))
    (test-assert (handler-case
                     (progn (devin--read-stream-frame stream) nil)
                   (devin-error () t))
                 "a mid-payload truncation is rejected")))

(-> test-devin-chat-request () boolean)
(defun test-devin-chat-request ()
  "The chat request carries metadata, prompts, tools, and no assignment JWT."
  (let* ((provider (devin-test--provider))
         (configuration (provider-configuration provider))
         (conversation (conversation-create configuration))
         (tools (vector
                 (json-object "name" "fs"
                              "tools" (json-array
                                       (json-object
                                        "name" "read"
                                        "description" "Read a file"
                                        "parameters" (json-object
                                                      "type" "object")))))))
    (conversation-append-user-message conversation "hello")
    (let ((request (devin--chat-request provider conversation tools
                                        :model-uid "model-uid"
                                        :user-jwt "user-jwt"
                                        :token "token")))
      (let ((reader (protobuf-reader-create request))
            (fields nil)
            (tool-names nil))
        (loop until (protobuf-reader-exhausted-p reader)
              do (multiple-value-bind (field wire-type) (protobuf-read-tag reader)
                   (push field fields)
                   (cond
                     ((and (= field 10) (= wire-type +protobuf-wire-length-delimited+))
                      (let* ((octets (protobuf-read-length-delimited reader))
                             (inner (protobuf-reader-create octets)))
                        (loop until (protobuf-reader-exhausted-p inner)
                              do (multiple-value-bind (f w) (protobuf-read-tag inner)
                                   (cond
                                     ((and (= f 1) (= w +protobuf-wire-length-delimited+))
                                      (push (protobuf-read-string inner) tool-names))
                                     (t (protobuf-skip-field inner w)))))))
                     (t (protobuf-skip-field reader wire-type)))))
        (test-assert (member 1 fields) "the request carries metadata")
        (test-assert (member 2 fields) "the request carries a system prompt")
        (test-assert (member 3 fields) "the request carries a prompt")
        (test-assert (member 7 fields) "the request carries the request type")
        (test-assert (member 8 fields) "the request carries completion config")
        (test-assert (member 10 fields) "the request carries tool definitions")
        (test-assert (member 15 fields) "the request carries a trajectory")
        (test-assert (member 16 fields) "the request carries a cascade id")
        (test-assert (member 20 fields) "the request carries planner mode")
        (test-assert (member 21 fields) "the request carries the model uid")
        (test-assert (not (member 26 fields))
                     "the request omits the assignment JWT")
        (test-assert (member "read" tool-names :test #'string=)
                     "tool names are flattened without a namespace prefix")))))

(-> test-devin-parse-chat-response () boolean)
(defun test-devin-parse-chat-response ()
  "The chat response parser accumulates text, thinking, and tool calls."
  (let ((provider (devin-test--provider))
        (text (make-string-output-stream))
        (thinking (make-string-output-stream))
        (tool-calls (make-hash-table :test 'equal))
        (events nil))
    ;; frame 1: response id + text delta + thinking delta
    (let ((frame (let ((b (protobuf--writer)))
                   (protobuf-write-string b 1 "bot-1")
                   (protobuf-write-string b 3 "hello")
                   (protobuf-write-string b 9 "thinking")
                   b)))
      (multiple-value-bind (id reason)
          (devin--parse-chat-response provider frame
                                      (lambda (e) (push e events))
                                      text thinking tool-calls)
        (test-assert (string= id "bot-1") "the response id is parsed")
        (test-assert (null reason) "no stop reason yet")))
    ;; frame 2: tool call start (id + name)
    (let ((frame (let ((b (protobuf--writer)))
                   (protobuf-write-message
                    b 6 (let ((inner (protobuf--writer)))
                          (protobuf-write-string inner 1 "call-1")
                          (protobuf-write-string inner 2 "get_weather")
                          inner))
                   b)))
      (devin--parse-chat-response provider frame
                                  (lambda (e) (push e events))
                                  text thinking tool-calls))
    ;; frame 3: args delta without id
    (let ((frame (let ((b (protobuf--writer)))
                   (protobuf-write-message
                    b 6 (let ((inner (protobuf--writer)))
                          (protobuf-write-string inner 3 "{\"city\":\"Par")
                          inner))
                   b)))
      (devin--parse-chat-response provider frame
                                  (lambda (e) (push e events))
                                  text thinking tool-calls))
    ;; frame 4: args delta + finish
    (let ((frame (let ((b (protobuf--writer)))
                   (protobuf-write-message
                    b 6 (let ((inner (protobuf--writer)))
                          (protobuf-write-string inner 3 "is\"}")
                          inner))
                   (protobuf-write-uint64 b 5 10)
                   b)))
      (multiple-value-bind (id reason)
          (devin--parse-chat-response provider frame
                                      (lambda (e) (push e events))
                                      text thinking tool-calls)
        (declare (ignore id))
        (test-assert (= reason 10) "the tool-calls stop reason is parsed")))
    (test-assert (string= (get-output-stream-string text) "hello")
                 "text deltas accumulate")
    (test-assert (string= (get-output-stream-string thinking) "thinking")
                 "thinking deltas accumulate")
    (let ((entry (gethash "call-1" tool-calls)))
      (test-assert (not (null entry)) "the tool call is registered")
      (test-assert (string= (getf entry :name) "get_weather")
                   "the tool name is parsed")
      (test-assert (string= (get-output-stream-string (getf entry :args))
                            "{\"city\":\"Paris\"}")
                   "argument deltas accumulate across frames"))))

(-> test-devin-tool-name-map () boolean)
(defun test-devin-tool-name-map ()
  "The tool name map resolves flat wire names to namespace and name."
  (let* ((provider (devin-test--provider))
         (tools (vector
                 (json-object "name" "fs"
                              "tools" (json-array
                                       (json-object "name" "read")
                                       (json-object "name" "write")))))
         (name-map (devin--tool-name-map provider tools)))
    (test-assert (equal (gethash "read" name-map) '("fs" "read"))
                 "a flat name resolves to its namespace")
    (test-assert (equal (gethash "write" name-map) '("fs" "write"))
                 "a second flat name resolves")
    (test-assert (null (gethash "missing" name-map))
                 "an unknown name has no mapping")))

(-> test-devin-model-uid () boolean)
(defun test-devin-model-uid ()
  "Friendly model names map to wire uids; the router resolves through AssignModel."
  (let ((provider (devin-test--provider)))
    (test-assert (string= (devin-provider--model-uid provider "devin/swe-1.7")
                          "swe-1-7")
                 "the swe model maps to its wire uid")
    (test-assert (string= (devin-provider--model-uid provider "devin/gpt-5.6-sol")
                          "gpt-5-6-sol-low")
                 "the gpt model maps to its wire uid")
    (test-assert (string= (devin-provider--model-uid provider "unknown-model")
                          "unknown-model")
                 "an unmapped model passes through")))

(-> test-devin-prompt-source () boolean)
(defun test-devin-prompt-source ()
  "Conversation roles map to Cascade message sources."
  (test-assert (= (devin--prompt-source "user") *devin-chat-message-source-user*)
               "user maps to the user source")
  (test-assert (= (devin--prompt-source "assistant") *devin-chat-message-source-system*)
               "assistant maps to the system source")
  (test-assert (= (devin--prompt-source "tool") *devin-chat-message-source-tool*)
               "tool maps to the tool source"))

(-> test-devin-item-text () boolean)
(defun test-devin-item-text ()
  "Conversation items yield their text content."
  (test-assert (string= (devin--item-text
                         (json-object "role" "user" "content" "hello"))
                        "hello")
               "a string content item yields its text")
  (test-assert (string= (devin--item-text
                         (json-object "role" "user"
                                      "content" (json-array
                                                 (json-object "type" "input_text"
                                                              "text" "hello"))))
                        "hello")
               "a structured content item yields its text"))
