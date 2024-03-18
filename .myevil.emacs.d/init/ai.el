
(use-package gptel
  :load-path (my-packages-directory "gptel")
  :commands (gptel-menu)
  :config
  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-ollama)
  (setq
   gptel-model "mistral:instruct"
   gptel-backend
   (gptel-make-ollama "ollama-mistral"
                      :host "localhost:11434"
                      :stream t
                      :models '("mistral:instruct"))
   )
)

(use-package llm
  :load-path (my-packages-directory "llm")
)

(use-package ellama
  :load-path (my-packages-directory "ellama")
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
                          :chat-model "mistral:instruct"
                          :embedding-model "mistral:instruct"))
  ;; (setopt ellama-providers
  ;;         '(("mistral" . (make-llm-ollama
  ;;                         :chat-model "mistral:instruct"
  ;;                         :embedding-model "mistral:instruct")))
  ;;         )
  )
