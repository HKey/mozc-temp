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
  (let ((convertibles "a-zA-Z-,.!?"))
    (format "\\(?:^\\|[^%s]\\)\\([%s]+\\)\\=" convertibles convertibles))
  "A regexp to specify the prefix string for conversion.
The prefix string is used as pre-input of mozc's conversion.

The default value means (\"|\" means the cursor position):
  hogehoge hugahuga|
           ^^^^^^^^
         prefix string")

(defvar mozc-temp-auto-conversion-p nil)

(defvar mozc-temp-remove-space-p t)

(defvar mozc-temp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap mozc-handle-event] #'mozc-temp--handle-event)
    map))

(defvar mozc-temp--should-exit nil
  "Non-nil means that `mozc-temp-mode' should exit.")

(defvar mozc-temp--space-overlay nil)

(defvar mozc-temp--prefix-overlay nil)

(defun mozc-temp--delete-overlay-region (overlay)
  "Delete the text in the region of OVERLAY."
  (when (overlayp overlay)
    (delete-region (overlay-start overlay)
                   (overlay-end overlay))))

(defun mozc-temp--done ()
  (mozc-temp--delete-overlay-region mozc-temp--prefix-overlay)
  (when mozc-temp-remove-space-p
    (undo-boundary)
    (mozc-temp--delete-overlay-region mozc-temp--space-overlay))
  (mozc-temp-mode -1))

(defun mozc-temp--handle-event (event)
  (interactive (list last-command-event))
  (let ((mozc-temp--should-exit nil))
    (prog1 (mozc-handle-event event)
      (when mozc-temp--should-exit
        (mozc-temp--done)))))

(defun mozc-temp--cleanup ()
  (--each (list mozc-temp--space-overlay mozc-temp--prefix-overlay)
    (when (overlayp it)
      (delete-overlay it)))
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


(defun mozc-temp--preedit-deleted (mozc-send-key-event-result)
  "Return non-nil if MOZC-SEND-KEY-EVENT-RESULT means that preedit characters have been deleted."
  (null (mozc-protobuf-get mozc-send-key-event-result 'preedit)))

(defun mozc-temp--conversion-completed (mozc-send-key-event-result)
  "Return non-nil if MOZC-SEND-KEY-EVENT-RESULT means that the conversion has been completed."
  (mozc-protobuf-get mozc-send-key-event-result 'result))

(defadvice mozc-send-key-event (after mozc-temp activate)
  (setq mozc-temp--should-exit
        (or (mozc-temp--conversion-completed ad-return-value)
            (mozc-temp--preedit-deleted ad-return-value))))

(defadvice mozc-fall-back-on-default-binding (after mozc-temp activate)
  (setq mozc-temp--should-exit t))

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
        (when (re-search-backward "\\w\\( \\)\\=" (point-at-bol) t)
          (-when-let* ((space-beginning (match-beginning 1))
                       (space-end (match-end 1)))
            (setq mozc-temp--space-overlay
                  (make-overlay space-beginning space-end))
            (when mozc-temp-remove-space-p
              (overlay-put mozc-temp--space-overlay 'invisible t))))))
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

;; Local Variables:
;; eval: (when (fboundp (quote flycheck-mode)) (flycheck-mode 1))
;; eval: (when (fboundp (quote flycheck-package-setup)) (flycheck-package-setup))
;; End:
