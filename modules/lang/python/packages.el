;; -*- lexical-binding: t; -*-

(unless (featurep! -server)
  (when (member (featurep! :server) '(nil t :anaconda))
    (use-package anaconda-mode
      :hook (python-mode
	     (python-mode . anaconda-eldoc-mode)))
    ;; FIXME: It would be nice if that could be done declaratively
    ;; through a glue layer, without anaconda needing to know about
    ;; company
    (when (featurep! :editing company :enabled)
      (use-package company-anaconda
	:after (company anaconda-mode)
	:config (add-to-list 'company-backends
			     '(company-anaconda :with company-capf))))))

(unless (featurep! -pythonic)
  (use-package pythonic
    :commands (pythonic-activate))

  (unless (featurep! -pyenv)
    (use-package pyenv-mode
      :hook python-mode)
    (use-package pyenv-mode-auto
      :after pyenv-mode))

  ;; We used to use `pyvenv', which provides `pyvenv-restart-python',
  ;; but that is incompatible with `anaconda-mode' over TRAMP (see
  ;; https://github.com/pythonic-emacs/anaconda-mode/issues/400), so
  ;; this is a re-implementation on top of pythonic that anaconda is
  ;; built on, and which already provides (de)activate commands
  (unless (featurep! -venv-restart)
    (use-package pythonic
      :config
      (defcustom pythonic-restart-python-on-activate t
	"If `t', inferior python process for affected buffers
will be restarted whenever `pythonic-activate' or
`pythonic-deactivate' is used to change the virtualenv"
	:type 'boolean)

      (defun pythonic-restart-python-on-activate (activate-func virtualenv)
	"Advice around `pythonic-activate'. After a new virtual
environment is activated, restart inferior python processes for
all affected buffers."
	(if pythonic-restart-python-on-activate
	    (let* ((old python-shell-virtualenv-root)
		   (local (local-variable-if-set-p 'python-shell-virtualenv-root))
		   (candidate-buffers (cond
				       ((and local (not (python-shell-get-buffer)))
					())
				       (local
					(list (current-buffer)))
				       (t
					(cl-loop for buffer in (buffer-list)
						 if (with-current-buffer buffer
						      (and (python-shell-get-buffer)
							   (equal old python-shell-virtualenv-root)))
						 collect buffer))))
		   (_ (funcall activate-func virtualenv))
		   (new python-shell-virtualenv-root))
	      (unless (equal old new)
		(cl-loop with seen = ()
			 for buffer in candidate-buffers
			 for procbuf = (with-current-buffer buffer
					 (get-buffer (python-shell-get-buffer)))
			 for process = (get-buffer-process procbuf)
			 for cmd = (when process (combine-and-quote-strings (process-command process)))
			 for dedicated = (with-current-buffer buffer
					   (string= (python-shell-get-process-name t)
						    (buffer-name procbuf)))
			 unless (or (not process)
				    (find procbuf seen)
				    (with-current-buffer buffer
				      (not (equal new python-shell-virtualenv-root))))
			 do (progn
			      (with-current-buffer procbuf
				(delete-process (get-buffer-process procbuf))
				(goto-char (point-max))
				(insert "\n\n"
					"###\n"
					(format "### Restarting in new virtualenv (%s)\n"
						python-shell-virtualenv-root)
					"###\n"
					"\n\n")
				(run-python cmd dedicated nil)
				(goto-char (point-max)))
			      (push procbuf seen)))))
	  (funcall activate-func virtualenv)))

      (advice-add #'pythonic-activate :around #'pythonic-restart-python-on-activate)
      (advice-add #'pythonic-deactivate :around #'pythonic-restart-python-on-activate))))
