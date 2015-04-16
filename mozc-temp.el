;;; mozc-temp.el --- Use mozc temporarily            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hiroki YAMAKAWA

;; Author: Hiroki YAMAKAWA <s06139@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "2.10.0") (mozc "20140802.56"))
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

(defvar mozc-temp--space-marker nil)

(defun mozc-temp--handle-event (event)
  (interactive (list last-command-event))
  (let ((mozc-temp--mozc-has-completed-conversion-p nil)
        (mozc-temp--mozc-has-fallen-back-p nil))
    (prog1 (mozc-handle-event event)
      (when (or mozc-temp--mozc-has-completed-conversion-p
                mozc-temp--mozc-has-fallen-back-p)
        (mozc-temp-mode -1)))))

(defun mozc-temp--remove-space ()
  (when mozc-temp--space-marker
    (save-excursion
      (goto-char mozc-temp--space-marker)
      (delete-char 1))))

;;;###autoload
(define-minor-mode mozc-temp-mode
  "Temporary mozc mode"
  :keymap mozc-temp-mode-map
  (if mozc-temp-mode
      (mozc-mode 1)
    (mozc-mode -1)
    (when mozc-temp-remove-space-p
      (undo-boundary)
      (mozc-temp--remove-space))
    (setq mozc-temp--space-marker nil)))


(defadvice mozc-send-key-event (after mozc-temp activate)
  (setq mozc-temp--mozc-has-completed-conversion-p
       (mozc-protobuf-get ad-return-value 'result)))

(defadvice mozc-fall-back-on-default-binding (after mozc-temp activate)
  (setq mozc-temp--mozc-has-fallen-back-p t))

(defun mozc-temp--search-prefix-backward ()
  (re-search-backward mozc-temp-prefix-regexp (point-at-bol) t))

;;;###autoload
(defun mozc-temp-convert ()
  "Convert the current word with mozc."
  (interactive)
  (-when-let* ((tail (point))
               (head (save-match-data
                       (and (save-excursion (mozc-temp--search-prefix-backward))
                            ;; move the cursor to the first matched group
                            (save-excursion
                              (re-search-backward
                               (regexp-quote (match-string 1)) nil t)))))
               (prefix (buffer-substring-no-properties head tail)))
    (undo-boundary)
    (delete-region head tail)
    (let ((point (save-excursion
                   (re-search-backward "\\w \\=" (point-at-bol) t))))
      (when point
        (setq mozc-temp--space-marker
              (set-marker (make-marker) (1+ point)))))
    (mozc-temp-mode 1)
    (-each (append (string-to-list prefix)
                   (when mozc-temp-auto-conversion-p
                     '(? )))
      #'mozc-temp--handle-event)))

;;;###autoload
(defun mozc-temp-convert-dwim ()
  (interactive)
  (if (save-excursion
        (save-match-data
          (mozc-temp--search-prefix-backward)))
      (mozc-temp-convert)
    (mozc-temp-mode 1)))

(provide 'mozc-temp)
;;; mozc-temp.el ends here
