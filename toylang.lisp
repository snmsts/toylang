(defpackage :toylang/toylang
  (:nicknames :toylang)
  (:use :cl)
  (:import-from :trivia)
  (:export :$file))

(in-package :toylang/toylang)
  
(defvar *reserved* ()
  "reserved keyword for this language it contain functions expected to return string list which are applied in order until success. ")
(defvar *separator* '(#\lf #\; #\Space #\tab #\$ #\[ #\] #\{ #\} #\# #\= #\. #\,
                      #\\));; specially treated
(defvar *file* nil)

(defun read-entire-file (file)
  (with-open-file (stream file)
    (when stream
      (let ((seq (make-array (file-length stream)
                             :element-type 'character
                             :fill-pointer t)))
        (setf (fill-pointer seq) (read-sequence seq stream))
        seq))))

(defun lex (str)
  "separate string into list by *separator*"
  (loop with str = (if (stringp str)
                       str
                       (string str))
     with part = nil
     with result = (list (list :newline :file *file* :line 1))
     with \\ = nil
     with len = (length str)
     with line = 1
     for i from 0 below len
     for c = (aref str i)
     do (cond ((and (eql c #\\)
                    (not \\))
               (setf \\ t))
              (\\
               (cond ((eql c #\\)
                      (let ((last (coerce (reverse part) 'string)))
                        (unless (zerop (length last)) 
                          (push last result))
                        (push c result)
                        (setf part nil)))
                     ((eql c #\lf)
                      (incf line))
                     (t (push c part)))
               (setf \\ nil))
              ((find c *separator*)
               (let ((last (coerce (reverse part) 'string)))
                 (unless (zerop (length last)) 
                   (push last result))
                 (push (if (eql c #\lf)
                           (list :newline :file *file* :line (incf line))
                           c)
                       result)
                 (setf part nil)))
              ((or (eql c #\()
                   (eql c #\")
                   (eql c #\|))
               (let (v
                     (last (coerce (reverse part) 'string))
                     (oldi i))
                 (unless (zerop (length last))
                   (push last result))
                 (setf part nil)
                 (multiple-value-setq (v i)
                   (read-from-string str t nil :start i :preserve-whitespace t))
                 (let ((read (subseq str oldi i)))
                   (incf line (count #\lf read))
                   (push (list :quote :c read)
                         result))
                 (decf i)))
              (t (push c part)))
     finally (let ((last (coerce (reverse part) 'string)))
               (unless (zerop (length last))
                 (push last result))
               (return (reverse result)))))

(defun preprocess (lexed)
  "ignore comment lines and white spaces"
  (loop with len = (length lexed)
     with mode
     with part
     with result
     for i from 0 below len
     for cur = (elt lexed i)
     do (cond ((and (null mode)
                    (ignore-errors (string-equal "//" cur :end2 2)))
               (setf result (cdr result)
                     mode :line-comment))
              ((and (null mode)
                    (eql #\# cur)
                    (ignore-errors (eql (first (first result)) :newline)))
               (setf mode :macro))
              ((and (null mode)
                    (ignore-errors (eql (first cur) :newline))
                    (eql #\\ (first result)))
               (setf result (cdr result)))
              ((or (eql #\space cur)
                        (eql #\tab cur))) ;;ignore spaces
              ((eql :line-comment mode)
               (when (ignore-errors (eql (first cur) :newline))
                 (push cur result)
                 (setf mode nil)))
              ((eql :macro mode)
               (push cur part)
               (when (ignore-errors (eql (first cur) :newline))
                 (push (list :macro :c (reverse (cdr part))) result)
                 (push cur result)
                 (setf part nil
                       mode nil)))
              (t (push cur result)))
     finally (return-from preprocess (nreverse result))))

(defun syntax (lex &optional mode (line 0) (file nil))
  (loop
     with result
     with part
     with len = (length lex)
     for i from 0 below len
     for s = (nth i lex)
     do (if (ignore-errors (eql (first s) :newline))
            (setf file (getf (cdr s) :file)
                  line (getf (cdr s) :line))
            (progn
              (setf s (cond
                        ((typep s 'string)
                         (list :symbol :c s :file file :line line))
                        ((atom s)
                         (list s :file file :line line))
                        (t s)))
              (push s part)
              (cond ((eql #\; (first s))
                     (push (reverse part) result)
                     (setf part nil))
                    ((or (eql (first s) #\[)
                         (eql (first s) #\{))
                     (multiple-value-bind (r s2)
                         (syntax (subseq lex (1+ i)) (cons s mode) line file)
                       (pop part)
                       (incf i s2)
                       (push `(,(first s) :c ,@r
                                :file ,(getf (cdr s) :file)
                                :line ,(getf (cdr s) :line))
                             part)))
                    ((or (eql (first s) #\])
                         (eql (first s) #\}))
                     (pop part)
                     (when part
                       (push (reverse part) result))
                     (return-from syntax (values (reverse result) (1+ i))))))
            )
     finally
       (when part (push (reverse part) result))
       (return-from syntax (values (reverse result) i))))
#|broken
(defmacro defcodegen (name &body clause)
  (let ((list (gensym)))
    `(progn
       (defun ,name (,list)
         (trivia:match ,list ,@clause))
       (setf *reserved* (remove ',name *reserved*))
       (setf *reserved* `(,@*reserved* ,',name))
       t)))

(defcodegen |codegen#use|
  ((list* #\# "use" package _) (format nil "(in-package ~A)" package)))
(defcodegen |codegen#inline|
  ((list* #\# (list* (quote quote) (trivia:guard inline (stringp inline)) _) _)
   (format nil "~A" (read-from-string inline))))
(defcodegen |codegensexp|
  ((list* (list (quote quote) string) _) string))
(defcodegen |codegenfunction|
  ((list* "function" (trivia:guard name (stringp name))
          (list (quote quote) lambda-list) (list* #\{ body) _)
   (format nil "(defun ~A ~A~% ~A)" name lambda-list
           (codegen body))))
(defcodegen |codegen{-recur|
  ((list* #\{ rest)
   (codegen rest)))
(defcodegen |codegen{|
  ((list* (list* #\{ body) _)
   (format nil "(progn ~A)"
           (codegen body))))
(defcodegen |codegenlf|
  ((list* (trivia:guard br (ignore-errors (eql (first br) :newline))) _)
   ""))
(defcodegen |codegenlet|
  ((list* "let" (list #\[ bindings) (list* #\{ body) _)
   (format nil "(let ~A ~A)"
           (loop with r
              for i in bindings
              initially (push "(" r)
              do (if (eql i #\,)
                     (push (format nil ")~%(") r)
                     (push i r))
              finally
                (push ")" r)
                (return (format nil "(~{~A~^ ~})" (reverse r))))
           (codegen body))))

(defun codegen (tree)
  (format nil "~{~A~%~}"
          (loop for i in tree
             for r = (loop for f in *reserved*
                        for r = (funcall f i)
                        when r
                        collect r
                        until r)
             unless r
             do (print (list :unhandled i))
             append r)))
|#

(defun $file (input-file)
  (let* ((*file* (truename input-file))
         (read (read-entire-file input-file))
         (lex (lex read))
         (pred (preprocess lex)))
    (values pred)))

($file (merge-pathnames "test.toy" (asdf:system-source-directory (asdf:find-system :toylang))))
