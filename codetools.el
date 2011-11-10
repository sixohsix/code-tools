;;
;; Copy and paste this into your .emacs.


;; Bind "C-c i" to reorder imports.
(defun reorder-imports (&optional b e)
  (interactive "r")
  (push-mark)
  (shell-command-on-region b e "reorder_imports" (current-buffer) 't)
  (pop-mark)
  )
(global-set-key (read-kbd-macro "C-c i") 'reorder-imports)
