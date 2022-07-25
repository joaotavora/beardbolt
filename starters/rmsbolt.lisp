;; Common Lisp beardbolt starter file

;; beardbolt ONLY DISASSEMBLES THE MAIN FUNCTION.
;; Please ensure you have a main function defined,
;; and place all your code inside of it!

;; Local Variables:
;; beardbolt-command: "sbcl"
;; End:

(defun main ()
  (defun add (a b)
    ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
    ;; (declare (type fixnum a b))
    (+ a b))
  (add 2 3))
