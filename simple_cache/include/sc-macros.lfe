(defmacro defnote (tag args)
  `(defun ,tag (,@args)
     (notify (quote ,tag) (list ,@args))))
