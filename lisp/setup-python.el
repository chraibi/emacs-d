(require 'python)
;; (require 'ipython)
(provide 'setup-python)
;;; setup-hlinum.el ends here
                                        ; http://www.masteringemacs.org/articles/2013/03/11/whats-new-emacs-24-3
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq python-indent-guess-indent-offset 1)
            (setq python-indent-offset 4)))


                                        
;; (autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

(package-initialize)
(elpy-enable)


(setq ipython-command "/usr/local/bin/ipython")


(setq
 python-shell-interpreter "/usr/local/bin/ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(defun flycheck-python-setup ()
  (flycheck-mode)
  )
(add-hook 'python-mode-hook #'flycheck-python-setup)


(setq py-python-command-args '( "--colors=Linux"))
