;;
;; Copy and paste this into your .emacs.


(defun sh-region-replace (command &optional b e)
  (interactive "r")
  (shell-command-on-region b e command (current-buffer) 't)
  )

;; Bind "C-c i" to reorder imports.
(global-set-key
 (read-kbd-macro "C-c i")
 (lambda (&optional b e) (interactive "r")
   (sh-region-replace "reorder_imports2" b e)))

;; Bind "C-c a" to alphabetize.
(global-set-key
 (read-kbd-macro "C-c a")
 (lambda (&optional b e) (interactive "r")
   (sh-region-replace "pyalphabetize" b e)))
