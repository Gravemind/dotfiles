
(use-package gptel
  :load-path (my-packages-directory "gptel")
  :disabled
  :commands (gptel gptel-send gptel-menu)
  :config
  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-ollama)
  (require 'gptel-rewrite)
  (require 'gptel-privategpt)
  (require 'gptel-context)
  ;; (setq
  ;;  gptel-model "llama3.1:8b-instruct-q5_K_M"
  ;;  gptel-backend
  ;;  (gptel-make-ollama "ollama"
  ;;                     :host "localhost:11434"
  ;;                     :stream t
  ;;                     :models '("llama3.1:8b-instruct-q5_K_M"))
  ;;  )

  (gptel-make-ollama
      "ollama"
    :host "localhost:11434"
    :stream t
    :models
    '(
      llama3.1:8b-instruct-q5_K_M
      ))

  (gptel-make-privategpt
   "localhost-11434"               ;Any name you want
   :protocol "http"
   :host "localhost:11434"
   :stream t
   :context t                            ;Use context provided by embeddings
   :sources t                            ;Return information about source documents
   :models

;; (require 'json)
;; (let ((url "https://api.openai.com/v1/models"))
;;   (with-current-buffer (url-retrieve-synchronously url)
;;     (goto-char (point-min))
;;     (re-search-forward "^\\s-*")
;;     (let ((response (json-read-from-string (buffer-substring (point) (point-max)))))
;;       (dolist (model response)
;;         (message model))))


   '(
     ;; curl -o - http://localhost:11434/v1/models | jq '.data.[].id | "\"\(.)\""' -r XCLIP
     ;; llama3.1:70b-instruct-q2_K
     ;; llama3.1:8b-instruct-q5_K_M
     ;; codestral:22b-v0.1-q5_K_M
     ;; codellama:13b-python-q5_K_M
     codellama:13b-instruct-q5_K_M
     ;; llava:13b

     ))



)

(use-package llm
  :load-path (my-packages-directory "llm")
  :disabled
)

(use-package ellama
  :load-path (my-packages-directory "ellama")
  :disabled
  :config
  (require 'llm-ollama)
  ;; (setopt ellama-provider
  ;;               (make-llm-ollama
  ;;                :chat-model "mistral:7b-instruct-v0.2-q5_K_M"
  ;;                :embedding-model "mistral:7b-instruct-v0.2-q5_K_M"))

  (let ((models
         '(
           "llama3.1:8b-instruct-q5_K_M"
           )))
    (setopt
     ellama-providers (mapcar
                       (lambda (model) (message model) (cons model (make-llm-ollama :chat-model model :embedding-model model)))
                       models)
     )
    )

  )
