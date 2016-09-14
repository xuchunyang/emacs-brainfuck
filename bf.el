;;; bf.el --- Translate Brainfuck into Elisp and evaluate that  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Keywords: brainfuck
;; Created: Tue Sep 13 14:26:59 CST 2016
;; Homepage: https://github.com/xuchunyang/emacs-brainfuck
;; Version: 1

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

;;                              _____________

;;                                  BF.EL

;;                               Chunyang Xu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Usage
;; .. 1.1 Function `(bf S)'
;; .. 1.2 Command `M-x bf-eval-buffer'
;; .. 1.3 Command `M-x bf-eval-region'
;; 2 Brainfuck code samples


;; An Emacs Lisp program that translates [Brainfuck] program to Emacs Lisp
;; program then evaluates that.


;; [Brainfuck] https://en.wikipedia.org/wiki/Brainfuck


;; 1 Usage
;; =======

;; 1.1 Function `(bf S)'
;; ~~~~~~~~~~~~~~~~~~~~~

;;   Evaluate string S as brainfuck code. Here is an example to print an
;;   `@' sign:

;;   (bf "++++++++[>++++++++<-]>.")
;;        -| @
;;        => 64

;;   Note the return value of `bf' is undefined.


;; 1.2 Command `M-x bf-eval-buffer'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Execute current buffer as BF code.


;; 1.3 Command `M-x bf-eval-region'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Execute the region as BF code.


;; 2 Brainfuck code samples
;; ========================

;;   Since it is not easy to write brainfuck code by hand, I would like to
;;   [steal] some code samples for testing.

;;   The familiar “Hello World” program in brainfuck:

;;   ++++++[>++++++++++++<-]>.
;;   >++++++++++[>++++++++++<-]>+.
;;   +++++++..+++.>++++[>+++++++++++<-]>.
;;   <+++[>----<-]>.<<<<<+++[>+++++<-]>.
;;   >>.+++.------.--------.>>+.

;;   The factorial generator:

;;   >++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]
;;   <[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
;;   [>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<
;;   [<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]
;;   <<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<
;;   [<[>+>+<<-]>.<<<<<]>.>>>>]


;; [steal] http://beautifulracket.com/bf/intro.html

;;; Code:


;; hmm, global variables are not very good
(defvar bf-vec (make-vector 30000 0))
(defvar bf-ptr 0)

(defun bf-read (s)
  "Read BF source code in string S as S-exp."
  (fillarray bf-vec 0)
  (setq bf-ptr 0)
  (let* ((body (mapconcat (lambda (c)
                            (pcase c
                              (?\[ "(bf-loop")
                              (?\] ")")
                              ((or ?> ?< ?+ ?- ?. ?,) (format "(bf-op %d)" c))))
                          (string-to-list s)
                          " "))
         (program (format "(bf-program %s)" body)))
    (pcase-let ((`(,sexp . ,index) (read-from-string program)))
      (unless (= index (length program))
        (signal 'end-of-file '("bf-read found unmatched []")))
      sexp)))

(defun bf-current-byte () (aref bf-vec bf-ptr))
(defun bf-set-current-byte (val) (aset bf-vec bf-ptr val))

(defmacro bf-program (&rest args)
  `(progn ,@args))

(defun bf-op (op)
  (pcase op
    (?> (setq bf-ptr (1+ bf-ptr)))
    (?< (setq bf-ptr (1- bf-ptr)))
    (?+ (bf-set-current-byte (1+ (bf-current-byte))))
    (?- (bf-set-current-byte (1- (bf-current-byte))))
    ;; (?. (message "BF writing: %c" (bf-current-byte)))
    (?. (write-char (bf-current-byte)))
    (?, (bf-set-current-byte (read-char "BF read a char: ")))))

(defmacro bf-loop (&rest args)
  `(while (/= (bf-current-byte) 0)
     ,@args))

;;;###autoload
(defun bf (s)
  "Evaluate string S as brainfuck code"
  (eval (bf-read s)))

;;;###autoload
(defun bf-eval-buffer (&optional buffer)
  "Execute current buffer as BF code."
  (interactive)
  (bf (with-current-buffer (or buffer (current-buffer))
        (buffer-string))))

;;;###autoload
(defun bf-eval-region (beg end)
  "Execute the region as BF code."
  (interactive "r")
  (bf (buffer-substring beg end)))

(provide 'bf)
;;; bf.el ends here
