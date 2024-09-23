;;; sml-ts-mode.el --- SML major-mode using tree-sitter -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/sml-ts-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 23 September 2024
;; Keywords: sml languages tree-sitter

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A major mode for Standard ML (SML) powered by tree-sitter.
;;
;; The tree-sitter grammar compatible with this package can be found at
;; https://github.com/MatthewFluet/tree-sitter-sml.
;;
;;; Code:

(require 'treesit)

(defcustom sml-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `sml-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'sml)


;;; Indentation

(defvar sml-ts-mode--indent-rules
  `((sml
     ((parent-is "source_file") column-0 0)
     ((node-is "}") standalone-parent 0)
     ((node-is ")") parent-bol 0)
     (catch-all parent-bol 0)))
  "Tree-sitter indent rules for SML.")


;;; Font-locking

(defconst sml-ts-mode--keywords
  '(;; Reserved Words Core
    "abstype" "and" "andalso" "as" "case" "datatype" "do" "else" "end"
    "exception" "fn" "fun" "handle" "if" "in" "infix" "infixr" "let"
    "local" "nonfix" "of" "op" "open" "orelse" "raise" "rec" "then"
    "type" "val" "while" "with" "withtype"
    ;; Reserved Words Modules
    "eqtype" "functor" "include" "sharing" "sig" "signature" "struct"
    "structure" "where")
  "SML keywords for tree-sitter font-locking.")

(defconst sml-ts-mode--builtins
  '()
  "SML builtin functions to tree-sitter font-locking.")

(defconst sml-ts-mode--operators
  '("=" "andalso" "orelse" "..." "#")
  "SML operators for tree-sitter font-locking.")

(defvar sml-ts-mode-feature-list
  '(( comment definition)
    ( keyword string)
    ( builtin constant function number property type)
    ( bracket delimiter operator variable))
  "Font-locking features for `treesit-font-lock-feature-list' in `sml-ts-mode'.")

(defvar sml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'sml
   :feature 'comment
   '([(block_comment) (line_comment)] @font-lock-comment-face)

   :language 'sml
   :feature 'string
   '([(string_scon) (char_scon)] @font-lock-string-face)

   :language 'sml
   :feature 'keyword
   `([,@sml-ts-mode--keywords] @font-lock-keyword-face
     ;; Misinterpreted identifiers, eg. val x = struct
     ;; See notes from highlights.scm
     ([(vid) (tycon) (strid) (sigid) (fctid)] @font-lock-warning-face
      (:match ,(rx-to-string `(seq bos (or ,@sml-ts-mode--keywords) eos))
              @font-lock-warning-face))
     ;; As an additional special case, The Defn of SML excludes `*` from tycon.
     ([(tycon)] @font-lock-warning-face
      (:match ,(rx bos "*" eos) @font-lock-warning-face)))

   :language 'sml
   :feature 'operator
   `([,@sml-ts-mode--operators] @font-lock-operator-face
     ((vid) @font-lock-operator-face
      (:match ,(rx bos (or "+" "-" "*" "/" "div" "mod"
                           "=" "<>" "<" ">" "<=" ">=" "not"
                           "^" "<<" ">>" "andb" "orb" "xorb" "notb"
                           "::" "@" "o" ":=" "!" "=>")
                   eos)
              @font-lock-operator-face)))

   :language 'sml
   :feature 'definition
   '((valbind
      pat: [(wildcard_pat) (vid_pat)] @font-lock-variable-name-face)
     (fvalbind
      (fmrule name: (_) @font-lock-function-name-face
              ;; FIXME(09/23/24): needs finesse
              arg: [(_)] :* @font-lock-variable-name-face))
     ;; Constructors
     ((vid) @font-lock-type-face
      (:match "^[A-Z].*" @font-lock-type-face))
     ;; Modules
     [(strid) (sigid) (fctid)] @font-lock-type-face)

   :language 'sml
   :feature 'constant
   `(((vid) @font-lock-constant-face
      ;; FIXME(09/23/24): not right
      (:match ,(rx bos (or "true" "false" "nil" "ref") eos)
              @font-lock-constant-face)))

   :language 'sml
   :feature 'type
   ;; XXX(09/23/24): types copied from highlights.scm, untested
   `((fn_ty "->" @font-lock-type-face)
     (tuple_ty "*" @font-lock-type-face)
     (paren_ty ["(" ")"] @font-lock-type-face)
     (tyvar_ty (tyvar) @font-lock-type-face)
     (record_ty
      ["{" "," "}"] @font-lock-type-face
      (tyrow [(lab) ":"] @font-lock-type-face) :?
      (ellipsis_tyrow ["..." ":"] @font-lock-type-face) :?)
     (tycon_ty
      (tyseq ["(" "," ")"] @font-lock-type-face) :?
      (longtycon) @font-lock-type-face))

   ;; :language 'sml
   ;; :feature 'builtin
   ;; `()
   ;; 
   ;; :language 'sml
   ;; :feature 'function
   ;; '()
   ;; 
   ;; :language 'sml
   ;; :feature 'property
   ;; '()
   ;; 
   ;; :language 'sml
   ;; :feature 'variable
   ;; '()

   :language 'sml
   :feature 'number
   '([(integer_scon) (word_scon) (real_scon)] @font-lock-number-face)

   :language 'sml
   :feature 'delimiter
   '(["." "," ":" ";" "|" "=>" ":>"] @font-lock-delimiter-face)

   :language 'sml
   :feature 'bracket
   `(["(" ")" "#[" "[" "]" "{" "}"] @font-lock-bracket-face))
  "Tree-sitter font-lock settings for SML.")


(defun sml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or NODE is not a defun node."
  (pcase (treesit-node-type node)
    (_ (treesit-node-text
        (treesit-node-child-by-field-name node "name")
        t))))


;; Copied from `sml-mode-syntax-table'
(defvar sml-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (mapc (lambda (c) (modify-syntax-entry c "_" st)) "._'")
    (mapc (lambda (c) (modify-syntax-entry c "." st)) ",;")
    ;; `!' is not really a prefix-char, oh well!
    (mapc (lambda (c) (modify-syntax-entry c "'"  st)) "~#!")
    (mapc (lambda (c) (modify-syntax-entry c "."  st)) "%&$+-/:<=>?@`^|")
    st)
  "The syntax table used in `sml-ts-mode'.")


;;;###autoload
(define-derived-mode sml-ts-mode prog-mode "SML"
  "Major mode for editing SML buffers using tree-sitter."
  :group 'sml
  :syntax-table sml-ts-mode-syntax-table

  (when (treesit-ready-p 'sml)
    (treesit-parser-create 'sml)

    ;; Settings copied from `sml-mode'
    (setq-local paragraph-separate
                (concat "\\([ \t]*\\*)?\\)?\\(" paragraph-separate "\\)"))
    (setq-local require-final-newline t)
    (setq-local electric-indent-chars
                (cons ?\; (if (boundp 'electric-indent-chars)
                              electric-indent-chars '(?\n))))
    (setq-local electric-layout-rules
                `((?\; . ,(lambda ()
                            (save-excursion
                              (skip-chars-backward " \t;")
                              (unless (or (bolp)
                                          (progn (skip-chars-forward " \t;")
                                                 (eolp)))
                                'after))))))
    ;; TODO(09/23/24): copy stuff from `sml-mode'
    ;; (setq-local prettify-symbols-alist sml-font-lock-symbols-alist)
    ;; (setq-local outline-regexp sml-outline-regexp)

    ;; Comments
    (setq-local parse-sexp-ignore-comments t)
    (setq-local comment-start "(* ")
    (setq-local comment-end " *)")
    (setq-local comment-start-skip "(\\*+\\s-*")
    (setq-local comment-end-skip "\\s-*\\*+)")
    (setq-local comment-quote-nested nil)

    ;; Font-Locking
    (setq-local treesit-font-lock-settings sml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list sml-ts-mode-feature-list)

    ;; Navigation
    (setq-local treesit-defun-type-regexp nil)
    (setq-local treesit-defun-name-function #'sml-ts-mode--defun-name)
    (setq-local treesit-thing-settings
                `((sml (text ,(rx (or "block_comment" "line_comment"
                                      "string_scon" "char_scon"))))))
    ;; (sexp (not ,(rx (or "{" "}" "[" "]" "(" ")" ","))))
    ;; (sentence nil)

    ;; TODO(09/23/24): Indentation
    (setq-local treesit-simple-indent-rules sml-ts-mode--indent-rules)

    ;; TODO(09/23/24): Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Module")
                  ("Function")
                  ("Variable")))

    (treesit-major-mode-setup)))


(when (fboundp 'derived-mode-add-parents)
  (derived-mode-add-parents 'sml-ts-mode '(sml-mode)))

(if (treesit-ready-p 'sml)
    (add-to-list 'auto-mode-alist '("\\.sml\\'" . sml-ts-mode)))



(provide 'sml-ts-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; sml-ts-mode.el ends here
