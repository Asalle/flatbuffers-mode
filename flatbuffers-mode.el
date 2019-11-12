;;; flatbuffers-mode.el --- major mode for editing flatbuffers.

;; Author: Alexandre Vassalotti <alexandre@peadrop.com>
;; Author: Asal Mirzaieva <asalle.kim@gmail.com>
;; Created: 12-Nov-2019
;; Version: 0.1
;; Keywords: google flatbuffers languages

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Installation:
;;   - Put `flatbuffers-mode.el' in your Emacs load-path.
;;   - Add this line to your .emacs file:
;;       (require 'flatbuffers-mode)
;;
;; You can customize this mode just like any mode derived from CC Mode.  If
;; you want to add customizations specific to flatbuffers-mode, you can use the
;; `flatbuffers-mode-hook'. For example, the following would make flatbuffers-mode
;; use 2-space indentation:
;;
;;   (defconst my-flatbuffers-style
;;     '((c-basic-offset . 2)
;;       (indent-tabs-mode . nil)))
;;
;;   (add-hook 'flatbuffers-mode-hook
;;     (lambda () (c-add-style "my-style" my-flatbuffers-style t)))
;;
;; Refer to the documentation of CC Mode for more information about
;; customization details and how to use this mode.
;;
;;; Code:

(require 'cc-mode)

(eval-when-compile
  (and (= emacs-major-version 24)
       (>= emacs-minor-version 4)
       (require 'cl))
  (require 'cc-langs)
  (require 'cc-fonts))

;; This mode does not inherit properties from other modes. So, we do not use
;; the usual `c-add-language' function.
(eval-and-compile
  (put 'flatbuffers-mode 'c-mode-prefix "flatbuffers-"))

(c-lang-defconst c-primitive-type-kwds
  flatbuffers '("bool" "byte" "ubyte" "short" "ushort" "int" "uint" "float" "long" "ulong" "double" "int8" "uint8" "int16" "uint16" "int32" "uint32| int64 " "uint64" "float32" "float64" "string"))

(c-lang-defconst c-class-decl-kwds
  flatbuffers '("table" "enum" "struct" "union"))

(c-lang-defconst c-constant-kwds
  flatbuffers '("true" "false"))

(c-lang-defconst c-other-decl-kwds
  flatbuffers '("namespace" "include"))

(c-lang-defconst c-identifier-ops
  ;; Handle extended identifiers like google.flatbuffers.MessageOptions
  flatbuffers '((left-assoc ":")))

;; Here we remove default syntax for loops, if-statements and other C
;; syntactic features that are not supported by the flatbuffers language.

(c-lang-defconst c-brace-list-decl-kwds
  ;; Remove syntax for C-style enumerations.
  flatbuffers nil)

(c-lang-defconst c-block-stmt-1-kwds
  ;; Remove syntax for "do" and "else" keywords.
  flatbuffers nil)

(c-lang-defconst c-block-stmt-2-kwds
  ;; Remove syntax for "for", "if", "switch" and "while" keywords.
  flatbuffers nil)

(c-lang-defconst c-simple-stmt-kwds
  ;; Remove syntax for "break", "continue", "goto" and "return" keywords.
  flatbuffers nil)

(c-lang-defconst c-paren-stmt-kwds
  ;; Remove special case for the "(;;)" in for-loops.
  flatbuffers nil)

(c-lang-defconst c-label-kwds
  ;; Remove case label syntax for the "case" and "default" keywords.
  flatbuffers nil)

(c-lang-defconst c-before-label-kwds
  ;; Remove special case for the label in a goto statement.
  flatbuffers nil)

(c-lang-defconst c-cpp-matchers
  ;; Disable all the C preprocessor syntax.
  flatbuffers nil)

(c-lang-defconst c-decl-prefix-re
  ;; Same as for C, except it does not match "(". This is needed for disabling
  ;; the syntax for casts.
  flatbuffers "\\([\{\};,]+\\)")


;; Add support for variable levels of syntax highlighting.

(defconst flatbuffers-font-lock-keywords-1 (c-lang-const c-matchers-1 flatbuffers)
  "Minimal highlighting for flatbuffers-mode.")

(defconst flatbuffers-font-lock-keywords-2 (c-lang-const c-matchers-2 flatbuffers)
  "Fast normal highlighting for flatbuffers-mode.")

(defconst flatbuffers-font-lock-keywords-3 (c-lang-const c-matchers-3 flatbuffers)
  "Accurate normal highlighting for flatbuffers-mode.")

(defvar flatbuffers-font-lock-keywords flatbuffers-font-lock-keywords-3
  "Default expressions to highlight in flatbuffers-mode.")

;; Our syntax table is auto-generated from the keyword classes we defined
;; previously with the `c-lang-const' macro.
(defvar flatbuffers-mode-syntax-table nil
  "Syntax table used in flatbuffers-mode buffers.")
(or flatbuffers-mode-syntax-table
    (setq flatbuffers-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table flatbuffers))))

(defvar flatbuffers-mode-abbrev-table nil
  "Abbreviation table used in flatbuffers-mode buffers.")

(defvar flatbuffers-mode-map nil
  "Keymap used in flatbuffers-mode buffers.")
(or flatbuffers-mode-map
    (setq flatbuffers-mode-map (c-make-inherited-keymap)))

(easy-menu-define flatbuffers-menu flatbuffers-mode-map
  "Flatbuffers Mode Commands"
  (cons "Flatbuffers" (c-lang-const c-mode-menu flatbuffers)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.fbs\\'" . flatbuffers-mode))

;;;###autoload
(defun flatbuffers-mode ()
  "Major mode for editing Flatbuffers description language.

The hook `c-mode-common-hook' is run with no argument at mode
initialization, then `flatbuffers-mode-hook'.

Key bindings:
\\{flatbuffers-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table flatbuffers-mode-syntax-table)
  (setq major-mode 'flatbuffers-mode
        mode-name "Flatbuffers"
        local-abbrev-table flatbuffers-mode-abbrev-table
        abbrev-mode t)
  (use-local-map flatbuffers-mode-map)
  (c-initialize-cc-mode t)
  (if (fboundp 'c-make-emacs-variables-local)
      (c-make-emacs-variables-local))
  (c-init-language-vars flatbuffers-mode)
  (c-common-init 'flatbuffers-mode)
  (easy-menu-add flatbuffers-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'flatbuffers-mode-hook)
  (c-update-modeline))

(provide 'flatbuffers-mode)

;;; flatbuffers-mode.el ends here
