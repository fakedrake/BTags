;; NOTE: remember that tags are symbols (when dealing with interactivity)
;; (intern "name") creates the symbol we want, connect it to

;; TODO
;; Tag orgering on switch
;;
;; Usage:
;; When you use ido-find-file the file you open is assigned tags according to its directory
;; /a/b/c/d.c will have tags c, b and a in descending importance order
;;
;; You can manually assign tags to the top of the current buffer(if the tag exists it is pushed to the top of priorities) by M-x btag-tag-current-buffer
;;
;; Tag activation bubbles up all the buffers that have the tag in the sequence they were last switched to. Activate a tag by calling M-x btag-activate-tag
;; In tag activation buffers who have the requested tag first in priority are prioritized
;; eg. activate tag b when in /a/b/c/d.c will actually change the buffer to /a/b/k.c even though the most recent buffer with the tag b is /a/b/c/d.c
;; 2nd, third etc places count as the same.
;;
;; You can remove a tag from the current buffer by doing M-x btag-remove-tag-current-buffer
;;
;; Clear all tags from current buffer with M-x btag-clear-current-buffer-tags
;;
;; This is intended to be used to switch the buffer list paradigm according to the directory you are into

(defvar btag-activation-history '())

(defvar btag-connect-list nil
  "An alist of connections between buffers and tags")

(defvar btag-buffer-priorities nil
  "An alist with the btags of each buffer")

(defun btag-tag-buffer (buffer tag)
  "Associate a tag with the buffer"
  ;; if buffer is not in the buffers encountered by btag add it
  (if (not (assoc buffer btag-buffer-priorities))
      (progn
	(setq btag-buffer-priorities (cons (cons buffer (cons tag nil)) btag-buffer-priorities))
	(setq btag-connect-list      (cons (cons tag buffer) btag-connect-list))
	)

    (progn
      ;; push tag to top and delete it if it is in the buffer's tags
      (setf (cdr (assoc buffer btag-buffer-priorities)) (cons tag (delete tag (cdr (assoc buffer btag-buffer-priorities)))))

      ;; if the tag is new
      (unless (assoc tag btag-connect-list)
	(progn
	  (setq btag-connect-list (cons (cons tag buffer) btag-connect-list))
	  )
	)
      )))

(defun btag-remove-tag (buffer tag)
  (setf (cdr (assoc buffer btag-buffer-priorities)) (delete tag (cdr (assoc buffer btag-buffer-priorities))))
  (setq btag-connect-list (delete (cons tag buffer) btag-connect-list)))

(defun btag-remove-tag-current-buffer (tag)
  (interactive "SBtag to be removed: ")
  (btag-remove-tag (current-buffer) tag))

(defun btag-clear-current-buffer-tags ()
  "Clear all tags from current buffer"
  (interactive)
  (loop for i in (cdr (assoc (current-buffer) btag-buffer-priorities)) do
	(btag-remove-tag (current-buffer) i)
	))

(defun btag-tag-current-buffer (tag)
  "Set the btag to the top of the current buffer's priorities"
  (interactive "SBTag name: ")
  (btag-tag-buffer (current-buffer) tag))

(defun btag-path-to-tag (buf)
  "Tag the buf buffer with the names of each of the parent directories"
  (if (and (not (cdr (assoc buf btag-buffer-priorities))) (buffer-file-name buf))
      (loop for i in (reverse (cdr (reverse (split-string (buffer-file-name buf) "/")))) do
	    (if (not (equal i ""))
		(btag-tag-buffer buf (intern i))
	      ))))

;; Buffers with this tag first have priority
(defun btag-activate-tag (tag)
  "Push up all buffers using this tag"
  (interactive "SActivate btag: ")
  (let ((clist-buffers (buffer-list))
	(tag-found nil))
    (loop for i in clist-buffers do
	  (unless (find tag (cdr (assoc i btag-buffer-priorities))) (progn
								      (setq tag-found t)
								      (bury-buffer i))))

    ;; Now bury the ones that do not have this one first in priority
    (loop for i in clist-buffers do
	  (unless (equal (car (assoc i btag-buffer-priorities)) tag)
	    (bury-buffer i)))

    (if (equal tag-found t)
	(switch-to-buffer (other-buffer))
      (message "Tag not found"))
    ))


(defun btag-clean-all ()
  (interactive)
  (setq btag-buffer-priorities nil)
  (setq btag-connect-list nil)
  )

(defun btag-list-according-to-context ()
  (delete-dups (mapcar (lambda (x) (symbol-name (car x))) btag-connect-list)))



(defun ido-btag-activate-tag ()
  (interactive)
  (let ((selected (ido-completing-read
		   "Activate tag: "
		   (btag-list-according-to-context)
		   nil t nil 'btag-activation-history nil)))
    (unless (equal selected nil)
      ;; (setq btag-activation-history (cons selected btag-activation-history))
      (btag-activate-tag (intern selected))
      )
    ))

(defun btag-show-buffer-tags ()
  "Output the tags associated with the current buffer"
  (interactive)
  (with-output-to-temp-buffer
      (format "*btags: %s*" (buffer-name (current-buffer)))
    (princ (format "Tags associated with buffer %s\n\n" (buffer-name (current-buffer))))
    (loop for i in (cdr (assoc (current-buffer) btag-buffer-priorities)) do
	  (princ (concat (symbol-name i) "\n")))))

(add-hook 'find-file-hook (lambda () (btag-path-to-tag (current-buffer))))
(add-hook 'kill-buffer-hook (lambda () (btag-clear-current-buffer-tags)))

(define-minor-mode btags-mode
  "A minor mode that allows for easy switching between categories of buffers.
Especially useful when working with more than one project."
  nil " dtag-mode"
  '(("\C-xt" . ido-btag-activate-tag))
  :global t)

(provide 'btags)
