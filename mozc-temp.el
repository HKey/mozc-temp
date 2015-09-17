;;; mozc-temp.el --- Use mozc temporarily            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "2.10.0") (mozc "0"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mozc)
(require 'dash)

(defvar mozc-temp-prefix-regexp
  "\\(?:^\\|[^a-zA-Z-,.!?]\\)\\([a-zA-Z-,.!?]+\\)\\=")

(defvar mozc-temp-auto-conversion-p nil)

(defvar mozc-temp-remove-space-p t)

(defvar mozc-temp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap mozc-handle-event] #'mozc-temp--handle-event)
    map))

(defvar mozc-temp--mozc-has-completed-conversion-p nil)

(defvar mozc-temp--mozc-has-fallen-back-p nil)

(defvar mozc-temp--space-overlay nil)

(defvar mozc-temp--prefix-overlay nil)

(defun mozc-temp--done ()
  (when (overlayp mozc-temp--prefix-overlay)
    (delete-region (overlay-start mozc-temp--prefix-overlay)
                   (overlay-end mozc-temp--prefix-overlay))
    (delete-overlay mozc-temp--prefix-overlay))
  (when mozc-temp-remove-space-p
    (undo-boundary)
    (mozc-temp--remove-space))
  (mozc-temp-mode -1))

(defun mozc-temp--handle-event (event)
  (interactive (list last-command-event))
  (let ((mozc-temp--mozc-has-completed-conversion-p nil)
        (mozc-temp--mozc-has-fallen-back-p nil))
    (prog1 (mozc-handle-event event)
      (when (or mozc-temp--mozc-has-completed-conversion-p
                mozc-temp--mozc-has-fallen-back-p)
        (mozc-temp--done)))))

(defun mozc-temp--remove-space ()
  (when (overlayp mozc-temp--space-overlay)
    (delete-region (overlay-start mozc-temp--space-overlay)
                   (overlay-end mozc-temp--space-overlay))
    (delete-overlay mozc-temp--space-overlay)))

(defun mozc-temp--cleanup ()
  (setq mozc-temp--space-overlay nil
        mozc-temp--prefix-overlay nil))

;;;###autoload
(define-minor-mode mozc-temp-mode
  "Temporary mozc mode"
  :keymap mozc-temp-mode-map
  (if mozc-temp-mode
      (mozc-mode 1)
    (mozc-mode -1)
    (mozc-temp--cleanup)))


(defadvice mozc-send-key-event (after mozc-temp activate)
  (setq mozc-temp--mozc-has-completed-conversion-p
        (mozc-protobuf-get ad-return-value 'result)))

(defadvice mozc-fall-back-on-default-binding (after mozc-temp activate)
  (setq mozc-temp--mozc-has-fallen-back-p t))

(defun mozc-temp--get-prefix ()
  (save-excursion
    (save-match-data
      (and (re-search-backward mozc-temp-prefix-regexp (point-at-bol) t)
           (match-string 1)))))

;;;###autoload
(defun mozc-temp-convert ()
  "Convert the current word with mozc."
  (interactive)
  (-when-let* ((tail (point))
               (prefix (mozc-temp--get-prefix))
               (head (save-match-data
                       (save-excursion
                         (re-search-backward (regexp-quote prefix) nil t)))))
    (undo-boundary)
    (setq mozc-temp--prefix-overlay (make-overlay head tail))
    (overlay-put mozc-temp--prefix-overlay 'invisible t)
    (save-match-data
      (save-excursion
        (goto-char head)
        (re-search-backward "\\w\\( \\)\\=" (point-at-bol) t)
        (-when-let* ((space-beginning (match-beginning 1))
                     (space-end (match-end 1)))
          (setq mozc-temp--space-overlay
                (make-overlay space-beginning space-end))
          (when mozc-temp-remove-space-p
            (overlay-put mozc-temp--space-overlay 'invisible t)))))
    (mozc-temp-mode 1)
    (-each (append (string-to-list prefix)
                   (when mozc-temp-auto-conversion-p
                     '(? )))
      #'mozc-temp--handle-event)))

;;;###autoload
(defun mozc-temp-convert-dwim ()
  (interactive)
  (if (mozc-temp--get-prefix)
      (mozc-temp-convert)
    (mozc-temp-mode 1)))

(provide 'mozc-temp)
;;; mozc-temp.el ends here
