(in-package "ACL2")

;; oh, how I would have loved to use std::extract-keywords
(defun kwd-list-to-alist (kwd-list)
  (cond ((endp (cdr kwd-list)) nil)
        ((keywordp (car kwd-list))
         (cons (cons (car kwd-list) (cadr kwd-list))
               (kwd-list-to-alist (cddr kwd-list))))
        (t (kwd-list-to-alist (cdr kwd-list)))))
