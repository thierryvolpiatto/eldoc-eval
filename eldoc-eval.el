;;; eldoc-eval.el -- Show eldoc when using M-:

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'eldoc)

;;; Minibuffer support.
;;  Enable displaying eldoc info in something else
;;  Than minibuffer when this one is in use.
;;
(defcustom eldoc-in-minibuffer t
  "Turn on eldoc in minibuffer."
  :group 'eldoc
  :type 'bolean)

(defcustom eldoc-in-minibuffer-show-fn 'eldoc-show-in-mode-line
  "A function to display eldoc info.
Should take one arg: the string to display"
  :group 'eldoc
  :type 'function)

(defcustom eldoc-show-in-mode-line-delay 12
  "The time we show eldoc when Emacs is idle."
  :group 'eldoc
  :type 'number)

(defcustom eval-prefered-function 'pp-eval-expression
  "Prefered function to use with `M-:'."
  :group 'lisp
  :type 'function)

(defcustom  eldoc-in-minibuffer-own-frame-p nil
  "Whether minibuffer have own frame or not."
  :group 'lisp
  :type 'boolean)

;;; Compatibility with Emacs-24.4
;; New implementation of eldoc in minibuffer that come
;; with Emacs-24.4 show the eldoc info of current-buffer while
;; minibuffer is in use, disable this and inline old Emacs behavior.

(and (boundp 'eldoc-message-function) (setq eldoc-message-function nil))

;;; Inline old definition (24.3)
;;
(defun eldoc-message (&rest args)
  (let ((omessage eldoc-last-message))
    (setq eldoc-last-message
	  (cond ((eq (car args) eldoc-last-message) eldoc-last-message)
		((null (car args)) nil)
		;; If only one arg, no formatting to do, so put it in
		;; eldoc-last-message so eq test above might succeed on
		;; subsequent calls.
		((null (cdr args)) (car args))
		(t (apply 'format args))))
    ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
    ;; are recorded in a log.  Do not put eldoc messages in that log since
    ;; they are Legion.
    ;; Emacs way of preventing log messages.
    (let ((message-log-max nil))
      (cond (eldoc-last-message (message "%s" eldoc-last-message))
	    (omessage (message nil)))))
  eldoc-last-message)

(defun eldoc-display-message-p ()
  (and (eldoc-display-message-no-interference-p)
       ;; If this-command is non-nil while running via an idle
       ;; timer, we're still in the middle of executing a command,
       ;; e.g. a query-replace where it would be annoying to
       ;; overwrite the echo area.
       (and (not this-command)
	    (symbolp last-command)
	    (intern-soft (symbol-name last-command)
			 eldoc-message-commands))))

(defun eldoc-display-message-no-interference-p ()
  (and eldoc-mode
       (not executing-kbd-macro)
       (not (and (boundp 'edebug-active) edebug-active))
       ;; Having this mode operate in an active minibuffer/echo area causes
       ;; interference with what's going on there.
       (not cursor-in-echo-area)
       (not (eq (selected-window) (minibuffer-window)))))

;; Internal.
(defvar eldoc-active-minibuffers-list nil
  "Store actives minibuffers with eldoc enabled.")
(defvar eldoc-mode-line-rolling-flag nil)

(defun eldoc-store-minibuffer ()
  "Store minibuffer buffer name in `eldoc-active-minibuffers-list'.
This function is called by each minibuffer started with eldoc support.
See `with-eldoc-in-minibuffer'."
  (with-selected-window (minibuffer-window)
    (push (buffer-name) eldoc-active-minibuffers-list)))

(defmacro with-eldoc-in-minibuffer (&rest body)
  "Enable eldoc support for minibuffer input that run in BODY."
  (declare (indent 0) (debug t))
  `(let ((timer (and eldoc-in-minibuffer
                     (run-with-idle-timer
                      eldoc-idle-delay
                      'repeat 'eldoc-mode-in-minibuffer))))
     (unwind-protect
         (minibuffer-with-setup-hook
             ;; When minibuffer is activated in body,
             ;; store it.
             'eldoc-store-minibuffer
           ,@body)
       (and timer (cancel-timer timer))
       ;; Each time a minibuffer exit or abort
       ;; his buffer is removed from stack,
       ;; assuming we can only exit the active minibuffer
       ;; on top of stack.
       (setq eldoc-active-minibuffers-list
             (cdr eldoc-active-minibuffers-list)))))

(defun eldoc-current-buffer ()
  "The `current-buffer' before activating minibuffer."
  (with-selected-frame (last-nonminibuffer-frame)
    (window-buffer
     (cond (eldoc-in-minibuffer-own-frame-p
            (selected-window))
           ((fboundp 'window-in-direction)
            (window-in-direction 
             'above (minibuffer-window)))
           (t (minibuffer-selected-window))))))

(defun eldoc-show-in-mode-line (str)
  "Display string STR in the mode-line next to minibuffer."
  (let (mode-line-in-non-selected-windows)
    (with-current-buffer (eldoc-current-buffer)
      (make-local-variable 'mode-line-format)
      (let ((mode-line-format (concat " " str)))
        (eldoc-maybe-roll-message-in-mode-line mode-line-format))
      (force-mode-line-update))))

(defun eldoc-maybe-roll-message-in-mode-line (str)
  (let* ((max (window-width (get-buffer-window (eldoc-current-buffer))))
         (len (length str))
         (tmp-str str))
    (if (and (> len max) eldoc-mode-line-rolling-flag)
        (while (sit-for 0.3)
           (setq tmp-str (substring tmp-str 2)
                 mode-line-format (concat tmp-str " [<]" str))
           (force-mode-line-update nil)
           (when (< (length tmp-str) 2) (setq tmp-str str)))
        (force-mode-line-update nil)
        (sit-for eldoc-show-in-mode-line-delay))))

(defun eldoc-mode-line-toggle-rolling ()
  (interactive)
  (setq eldoc-mode-line-rolling-flag (not eldoc-mode-line-rolling-flag)))
(define-key minibuffer-local-map (kbd "<C-M-right>") 'eldoc-mode-line-toggle-rolling)

(defun eldoc-mode-in-minibuffer ()
  "Show eldoc for current minibuffer input."
  (let ((buf (with-selected-window (minibuffer-window)
               (buffer-name))))
    ;; If this minibuffer have been started with
    ;;`with-eldoc-in-minibuffer' give it eldoc support
    ;; and update mode-line, otherwise do nothing.
    (condition-case err
        (when (member buf eldoc-active-minibuffers-list)
          (let* ((str-all (with-current-buffer buf
                            (minibuffer-completion-contents)))
                 (sym     (when str-all
                            (with-temp-buffer
                              (insert str-all)
                              (goto-char (point-max))
                              (unless (looking-back ")\\|\"")
                                (forward-char -1))
                              (eldoc-current-symbol))))
                 (info-fn (eldoc-fnsym-in-current-sexp))
                 (doc     (or (eldoc-get-var-docstring sym)
                              (eldoc-get-fnsym-args-string
                               (car info-fn) (cadr info-fn)))))
            (when doc (funcall eldoc-in-minibuffer-show-fn doc))))
      (scan-error nil)
      (beginning-of-buffer nil)
      (error (message "Eldoc in minibuffer error: %S" err)))))

(defun eval-expression-with-eldoc ()
  "Eval expression with eldoc support in mode-line."
  (interactive)
  (with-eldoc-in-minibuffer
    (call-interactively eval-prefered-function)))

;; Bind it to `M-:'.
(global-set-key [remap eval-expression] 'eval-expression-with-eldoc)


(provide 'eldoc-eval)
;;; eldoc-eval.el ends here
