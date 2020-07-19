;;; flatbuffers-mode.el --- major mode for editing flatbuffers.

;; Author: Asal Mirzaieva <asalle.kim@gmail.com>
;; Created: 12-Nov-2019
;; Version: 0.2
;; Keywords: flatbuffers languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Installation:
;;   - Put `flatbuffers-mode.el' in your Emacs load-path.
;;   - Add this line to your .emacs file:
;;       (require 'flatbuffers-mode)
;;
;;; Code:

;; Font-locking definitions and helpers
(defconst flatbuffers-mode-keywords
  '("namespace" "root_type" "struct" "table" "enum" "union" "required" "include" "attribute" "rpc_service" "file_extension" "file_identifier"))

(defconst flatbuffers-special-types
  '("double" "bool" "uint" "ulong"))

(defconst comment-start "//")

(defconst flatbuffers-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defconst flatbuffers-re-generic
  (concat "<[[:space:]]*'" flatbuffers-re-ident "[[:space:]]*>"))

(defun flatbuffers-re-word (inner) (concat "\\<" inner "\\>"))
(defun flatbuffers-re-grab (inner) (concat "\\(" inner "\\)"))
(defun flatbuffers-re-shy (inner) (concat "\\(?:" inner "\\)"))
(defun flatbuffers-re-item-def (itype)
  (concat (flatbuffers-re-word itype)
	  (flatbuffers-re-shy flatbuffers-re-generic) "?"
	  "[[:space:]]+" (flatbuffers-re-grab flatbuffers-re-ident)))

(defvar flatbuffers-mode-font-lock-keywords
  (append
   `(
     ;; Keywords
     (,(regexp-opt flatbuffers-mode-keywords 'symbols) . font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt flatbuffers-special-types 'symbols) . font-lock-type-face)

     (,(concat (flatbuffers-re-grab flatbuffers-re-ident) "[[:space:]]*:[^:]") 1 font-lock-variable-name-face)

     ;; Ensure we highlight `Foo' in a:Foo
     (,(concat ":[[:space:]]*" (flatbuffers-re-grab flatbuffers-re-ident)) 1 font-lock-type-face))

    ;; Ensure we highlight `Foo` in `struct Foo` as a type.
    (mapcar #'(lambda (x)
                (list (flatbuffers-re-item-def (car x))
                      1 (cdr x)))
            '(("enum" . font-lock-type-face)
              ("struct" . font-lock-type-face)
              ("union" . font-lock-type-face)
              ("root_type" . font-lock-type-face)
              ("table" . font-lock-type-face)))))

(defvar flatbuffers-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14")
    table))

;;;###autoload
(define-derived-mode flatbuffers-mode prog-mode "Flatbuffers"
  "Major mode for Flatbuffers code.

\\{flatbuffers-mode-map}"
  :group 'flatbuffers-mode
  :syntax-table flatbuffers-mode-syntax-table

  ;; Fonts
  (setq-local font-lock-defaults '(flatbuffers-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function . flatbuffers-mode-syntactic-face-function)))
  (font-lock-add-keywords nil `((,(concat comment-start ".*") 0 font-lock-comment-face t))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fbs\\'" . flatbuffers-mode))

(defun flatbuffers-mode-reload ()
  (interactive)
  (unload-feature 'flatbuffers-mode)
  (require 'flatbuffers-mode)
  (flatbuffers-mode))

(provide 'flatbuffers-mode)

;;; flatbuffers-mode.el ends here
