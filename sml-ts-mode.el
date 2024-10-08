;;; sml-ts-mode.el --- SML major-mode using tree-sitter -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/sml-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (sml-mode "6.12"))
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
(require 'sml-mode)                     ; faces, pretty symbols, outline-regexp


(defcustom sml-ts-mode-indent-offset 2
  "Number of spaces for each indentation step."
  :type 'integer
  :safe 'integerp
  :group 'sml)

(defcustom sml-ts-mode-indent-args 2
  "Number of spaces to indent args on separate line."
  :type 'integer
  :safe 'integerp
  :group 'sml)

(defcustom sml-ts-mode-indent-module 2
  "Number of spaces to indent structure and functor bodies."
  :type 'integer
  :safe 'integerp
  :group 'sml)


;;; Indentation

(defun sml-ts-mode--anchor-list-closer (&rest args)
  "Return the position to anchor a closing list bracket.
When there are arguments following the opening bracket on the same line, align
the closing bracket with the opener. Otherwise, align the closing bracket with
its standalone-parent. See `treesit-simple-indent-rules' for details of ARGS."
  (save-excursion
    (goto-char
     (apply (alist-get 'first-sibling treesit-simple-indent-presets) args))
    (forward-char 1)
    (if (looking-at-p "\\s-*$")
        (apply (alist-get 'parent-bol treesit-simple-indent-presets)
               args)
      (point))))

(defun sml-ts-mode--indent-pipe (&rest _)
  "Calculate indent offset for \"|\"."
  (max 0 (- sml-ts-mode-indent-offset 2)))

(defun sml-ts-mode--indent-args (&rest _)
  "Calculate indent offest for args."
  (+ sml-ts-mode-indent-offset sml-ts-mode-indent-args))

(defvar sml-ts-mode--indent-rules
  `((sml
     ((parent-is "source_file") column-0 0)
     ((node-is "}") standalone-parent 0)
     ((node-is ")") sml-ts-mode--anchor-list-closer 0)
     ((node-is "]") sml-ts-mode--anchor-list-closer 0)
     ((match "end" ,(rx bos (or "sig_sigexp" "struct_strexp"))) parent-bol 0)
     ((node-is "struct") parent-bol 0)
     ((node-is "sig") parent-bol 0)
     ;; XXX(10/08/24): add option to align 'else if'?
     ((node-is ,(rx bos (or "in" "end" "then" "else" "do") eos)) parent 0)
     ((node-is ,(rx bos "handle" eos)) parent 0)
     ((parent-is "handle_exp") parent sml-ts-mode-indent-offset)
     ((parent-is "fn_exp") parent sml-ts-mode-indent-offset)
     ((parent-is "^mrule") grand-parent sml-ts-mode-indent-offset)
     ((match nil "fmrule" "arg") parent-bol sml-ts-mode--indent-args)
     ;; Cases
     ((match "|" "case_exp") parent sml-ts-mode--indent-pipe)
     ((parent-is ,(rx bos (or "let_exp" "case_exp")))
      parent sml-ts-mode-indent-offset)
     ;; Align fun names in patterns
     ((match "|" "fvalbind") parent-bol 2)
     ((match "|" ,(rx bos (or "datbind")))
      parent-bol sml-ts-mode--indent-pipe)
     ((parent-is ,(rx bos (or "fmrule" "fvalbind" "datbind" "valbind"
                              "cond_exp" "sig_sigexp" "iter_exp"
                              "local_strdec")
                      eos))
      parent-bol sml-ts-mode-indent-offset)
     ;; Structures / Functors
     ((parent-is "struct_strexp") parent-bol sml-ts-mode-indent-module)
     ;; List Types
     ((match nil ,(rx bos (or "sequence_exp" "paren_exp" "list_exp"
                              "record_exp" "record_pat"
                              "tuple_exp" "tuple_pat" "app_pat"))
             nil 1 1)
      parent-bol sml-ts-mode-indent-offset)
     ((parent-is ,(rx (or "sequence_exp" "list_exp"
                          "record_exp" "record_pat"
                          "tuple_exp" "tuple_pat" "app_pat")))
      (nth-sibling 1) 0)
     ((parent-is "paren_exp") first-sibling 0)
     ((match nil "app_exp" nil 1 1)
      parent sml-ts-mode-indent-offset)
     ((parent-is "app_exp") (nth-sibling 1) 0)
     ;; XXX(10/08/24): indent block comments?
     ((parent-is "block_comment") no-indent)
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
  '("abs" "app" "before" "ceil" "chr" "concat" "exnMessage" "exnName" "explode"
    "floor" "foldl" "foldr" "getOpt" "hd" "ignore" "implode" "isSome" "length"
    "map" "null" "ord" "print" "real" "rev" "round" "size" "str" "substring"
    "tl" "trunc" "valOf" "vector")
  "SML builtin functions for tree-sitter font-locking.")

;; Ops in named nodes: "=" "andalso" "orelse" "..." "#"
(defconst sml-ts-mode--operators
  '("+" "-" "*" "/" "div" "mod"
    "=" "<>" "<" ">" "<=" ">=" "not"
    "^" "<<" ">>" "andb" "orb" "xorb" "notb"
    "::" "@" "o" ":=" "!" "=>")
  "SML operators for tree-sitter font-locking.")

(defvar sml-ts-mode-feature-list
  '(( comment definition)
    ( keyword string)
    ( builtin constant number type property)
    ( bracket delimiter operator))
  "Font-lock features for `treesit-font-lock-feature-list' in `sml-ts-mode'.")

(defun sml-ts-mode--fontify-params (node override start end &rest _args)
  "Fontify function arguments in NODE.
For a description of OVERRIDE, START, and END, see `treesit-font-lock-rules'."
  (pcase (treesit-node-type node)
    ((or "typed_pat" "paren_pat" "tuple_pat")
     (sml-ts-mode--fontify-params
      (treesit-node-child node 0 t) override start end))
    ("app_pat"
     ;; Fontify both bindings in (h::t)
     (when (equal "::" (treesit-node-text (treesit-node-child node 1)))
       (sml-ts-mode--fontify-params
        (treesit-node-child node 0 t) override start end))
     (sml-ts-mode--fontify-params
      (treesit-node-child node 1 t) override start end))
    ((or "vid_pat" "wildcard_pat")
     (treesit-fontify-with-override
      (treesit-node-start node) (treesit-node-end node)
      'font-lock-variable-name-face
      override start end))))

(defvar sml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'sml
   :feature 'comment
   '([(block_comment) (line_comment)] @font-lock-comment-face)

   :language 'sml
   :feature 'string
   '((string_scon) @font-lock-string-face
     (char_scon) @font-lock-constant-face)

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
   `(["=" "andalso" "orelse" "..." "#"] @font-lock-operator-face
     ((vid) @font-lock-operator-face
      (:match ,(rx-to-string `(seq bos (or ,@sml-ts-mode--operators) eos))
              @font-lock-operator-face)))

   :language 'sml
   :feature 'builtin
   `(((vid_exp) @font-lock-builtin-face
      (:match ,(rx-to-string `(seq bos (or ,@sml-ts-mode--builtins) eos))
              @font-lock-builtin-face))
     ((vid_exp) @font-lock-preprocessor-face
      (:match ,(rx bos (or "use") eos) @font-lock-preprocessor-face)))

   :language 'sml
   :feature 'property
   `((record_exp (exprow (lab) @font-lock-property-name-face))
     (recordsel_exp (lab) @font-lock-property-name-face))
   
   :language 'sml
   :feature 'definition
   (let ((pats '((app_pat) (paren_pat) (vid_pat) (tuple_pat) (wildcard_pat))))
     `((fmrule
        name: (_) @font-lock-function-name-face
        ([,@pats] :* @sml-ts-mode--fontify-params :anchor "=") :?)
       (mrule ([,@pats] :* @sml-ts-mode--fontify-params :anchor "=>"))
       (handle_exp (mrule (_) @sml-ts-mode--fontify-params))
       (labvar_patrow [(vid)] @font-lock-variable-name-face)
       (tuple_pat (_) @sml-ts-mode--fontify-params
                  ("," (vid_pat) @sml-ts-mode--fontify-params) :*)
       (app_pat (_) (_) @sml-ts-mode--fontify-params)
       (valbind pat: (_) @sml-ts-mode--fontify-params)
       (valdesc name: (vid) @font-lock-variable-name-face)
       ;; Modules, Sigs
       (fctbind name: (_) @font-lock-module-def-face
                arg: (strid) :? @font-lock-variable-name-face)
       (strbind name: (_) @font-lock-module-def-face)
       (sigbind name: (_) @font-lock-module-def-face)
       ;; Types
       (datatype_dec (datbind name: (tycon) @font-lock-type-def-face))
       (type_dec (typbind name: (tycon) @font-lock-type-def-face))
       (typedesc name: (_) @font-lock-type-def-face)
       ((vid) @font-lock-type-face
        (:match "^[A-Z].*" @font-lock-type-face))))

   :language 'sml
   :feature 'constant
   `(((vid) @font-lock-constant-face
      (:match ,(rx bos (or "true" "false" "nil" "ref") eos)
              @font-lock-constant-face)))

   :language 'sml
   :feature 'type
   `((fn_ty "->" @font-lock-type-face)
     (tuple_ty "*" @font-lock-type-face)
     (paren_ty ["(" ")"] @font-lock-type-face)
     (tyvar) @font-lock-type-face
     [(strid) (sigid) (fctid)] @font-lock-type-face
     (record_ty ["{" "," "}"] @font-lock-type-face
                (tyrow [(lab) ":"] @font-lock-type-face) :?
                (ellipsis_tyrow ["..." ":"] @font-lock-type-face) :?)
     (tycon_ty (tyseq ["(" "," ")"] @font-lock-type-face) :?
               (longtycon) @font-lock-type-face))

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
    ("fun_dec"
     (sml-ts-mode--defun-name
      (treesit-node-child
       (treesit-node-child node 0 t) 0 t)))
    ((or "structure_strdec" "datatype_dec" "signature_sigdec" "functor_fctdec")
     (sml-ts-mode--defun-name (treesit-node-child node 0 t)))
    (_ (treesit-node-text
        (treesit-node-child-by-field-name node "name")
        t))))

;; From `sml-mode-syntax-table'
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

(defconst sml-ts-mode-prettify-symbols-alist
  (cons '("::" . ?∷)
        sml-font-lock-symbols-alist)
  "Symbols to prettify in `sml-ts-mode'.")

;;;###autoload
(define-derived-mode sml-ts-mode prog-mode "SML"
  "Major mode for editing SML buffers using tree-sitter."
  :group 'sml
  :syntax-table sml-ts-mode-syntax-table

  (when (treesit-ready-p 'sml)
    (treesit-parser-create 'sml)

    ;; Comments
    (setq-local parse-sexp-ignore-comments t)
    (setq-local comment-start "(* ")
    (setq-local comment-end " *)")
    (setq-local comment-start-skip "(\\*+\\s-*")
    (setq-local comment-end-skip "\\s-*\\*+)")
    (setq-local comment-quote-nested nil)

    ;; Settings from `sml-mode'
    (setq-local outline-regexp sml-outline-regexp)
    (setq-local paragraph-separate
                (concat "\\([ \t]*\\*)?\\)?\\(" paragraph-separate "\\)"))
    (setq-local require-final-newline t)
    (setq-local prettify-symbols-alist sml-ts-mode-prettify-symbols-alist)

    ;; Font-Locking
    (setq-local treesit-font-lock-settings sml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list sml-ts-mode-feature-list)

    ;; Indentation
    (setq-local electric-indent-chars
                (append "|{}().;,"
                        (if (boundp 'electric-indent-chars)
                            electric-indent-chars
                          '(?\n))))
    (setq-local electric-layout-rules
                `((?. . after) (?\{ . after) (?| . after)
                  (?\} . before) (?\) . before)
                  (?\; . ,(lambda ()
                            (save-excursion
                              (skip-chars-backward " \t;")
                              (unless (or (bolp)
                                          (progn (skip-chars-forward " \t;")
                                                 (eolp)))
                                'after))))))
    (setq-local treesit-simple-indent-rules sml-ts-mode--indent-rules)

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (rx bos (or "fun_dec" "fmrule"
                            "structure_strdec" "datatype_dec"
                            "signature_sigdec" "functor_fctdec")))
    (setq-local treesit-defun-name-function #'sml-ts-mode--defun-name)
    (setq-local treesit-thing-settings
                `((sml (text ,(rx (or "block_comment" "line_comment"
                                      "string_scon" "char_scon")))
                       (sentence
                        ,(rx bos (or "case" "if" "then" "else" "let" "in"
                                     "fun_dec" "val_dec" "type_dec"
                                     "structure_strdec" "signature_sigdec"
                                     "functor_fctdec" "val_spec"
                                     "fmrule" "mrule" "conbind")
                             eos)))))

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`fun_dec\\'")
                  ("Sig" "\\`signature_sigdec\\'")
                  ("Struct" "\\`structure_strdec\\'")
                  ("Datatype" "\\`datatype_dec\\'")
                  ("Functor" "\\`functor_fctdec\\'")))

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
