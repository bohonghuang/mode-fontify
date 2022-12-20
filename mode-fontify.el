;;; mode-fontify.el --- Fontify buffer text by `major-mode' -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides some commands for highlighting the specific `major-mode' syntax in a region, while the buffer is in another `major-mode'.

;;; Code:

(require 'cl-lib)

(defcustom mode-fontify-function #'font-lock-default-fontify-region
  "Fontify function used in `mode-fontify-text'."
  :group 'mode-fontify
  :type '(choice
          (const :tag "Font-lock" font-lock-default-fontify-region)
          (function :tag "Custom")))

(defcustom mode-fontify-inhibit-hook-p nil
  "Non-nil means mode hooks are inhibited when fontifying text."
  :group 'mode-fontify
  :type 'boolean)

(defcustom mode-fontify-reindent-p nil
  "Non-nil means `indent-region' will be called before fontifying text."
  :group 'mode-fontify
  :type 'boolean)

(defun mode-fontify-text (mode text)
  "Use MODE to fontify TEXT with `mode-fontify-function'."
  (with-temp-buffer
    (erase-buffer)
    (insert text)
    (if mode-fontify-inhibit-hook-p
        (delay-mode-hooks (funcall mode))
      (funcall mode))
    (when mode-fontify-reindent-p
      (indent-region (point-min) (point-max)))
    (font-lock-default-function mode)
    (funcall mode-fontify-function (point-min) (point-max) nil)
    (buffer-string)))

(defun mode-fontify-fontified-text (text)
  "Make TEXT fontified.
It can avoid the face being changed by the syntax of the current buffer."
  (cl-loop for end = (next-single-property-change 0 'face text) then (next-single-property-change end 'face text)
           for beg = 0 then end
           if end
           do (put-text-property beg end 'font-lock-face (get-text-property end 'face text) text)
           else
           do (add-text-properties 0  (length text) '(fontified t) text)
           and return text))

(defun mode-fontify-select-mode ()
  "Select a `major-mode' in minibuffer."
  (intern (completing-read "Select mode: "
                           (cl-remove-duplicates
                            (let ((modes (cl-remove-if-not #'symbolp (mapcar #'cdr auto-mode-alist))))
                              (if (and (<= 29 emacs-major-version) (treesit-available-p))
                                  (nconc modes (mapcar (lambda (mode) (intern (replace-regexp-in-string "-mode$" "-ts-mode" (symbol-name mode)))) modes))
                                modes)))
                           nil nil nil nil nil)))

(defun mode-fontify-select-faces ()
  "Select faces using minibuffer."
  (read-face-name "Select face" (or (face-at-point) 'default) t))

(defun mode-fontify-region (mode beg end)
  "Use MODE to fontify the region (BEG . END)."
  (interactive (list (mode-fontify-select-mode) (region-beginning) (region-end)))
  (let ((buffer-read-only nil))
    (remove-text-properties beg end '(face))
    (remove-text-properties beg end '(font-lock-face))
    (remove-text-properties beg end '(fontified))
    (goto-char beg)
    (insert (funcall (if font-lock-mode #'mode-fontify-fontified-text #'identity)
                     (mode-fontify-text mode (prog1 (buffer-substring beg end)
                                               (delete-region beg end)))))))

(defun mode-fontify-face-region (mode faces beg end)
  "Use MODE to fontify the region (BEG . END).
The text from the region is grouped and highlighted if its face exists in FACES."
  (interactive (append (list (mode-fontify-select-mode)
                             (mode-fontify-select-faces))
                       (if (region-active-p)
                           (prog1 (list (region-beginning) (region-end))
                             (deactivate-mark))
                         (list (point-min) (point-max)))))
  (setq faces (mapcar (lambda (face) (and (not (eq face 'default)) face)) faces))
  (let ((regions (cl-loop for p from beg to end
                          do (goto-char p)
                          unless (and (member (face-at-point) faces)
                                      (or (> p beg) (not (looking-at-p "[[:blank:]]"))))
                          if (= beg p) do (setq beg (1+ p))
                          else collect (cons beg p) into regions and do (setq beg (1+ p))
                          finally
                          (cl-decf p)
                          (when (< beg p) (push (cons beg p) regions))
                          (cl-return regions))))
    (cl-loop for (beg . end) in regions
             do (mode-fontify-region mode beg end))))

(defun mode-fontify-face-region-at-point (mode)
  "Use MODE to fontify a block of text according to the face at point."
  (interactive (list (mode-fontify-select-mode)))
  (let ((beg (previous-single-property-change (point) 'face))
        (end (next-single-property-change (point) 'face)))
    (mode-fontify-region mode beg end)))

(provide 'mode-fontify)

;;; mode-fontify.el ends here
