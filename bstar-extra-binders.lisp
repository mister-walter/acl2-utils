(in-package "ACL2")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "centaur/bitops/part-select" :dir :system)

;; Take a "bits-spec" and turn it into a binding
;; A bits-spec is a list consisting of a symbol followed by keyword arguments
;; that part-select understands.

;; oh, how I would have loved to use std::extract-keywords
(defun kwd-list-to-alist (kwd-list)
  (cond ((endp (cdr kwd-list)) nil)
        ((keywordp (car kwd-list))
         (cons (cons (car kwd-list) (cadr kwd-list))
               (kwd-list-to-alist (cddr kwd-list))))
        (t (kwd-list-to-alist (cdr kwd-list)))))

(defun calculate-bits-spec-width (bits-spec)
  (b* ((kwd-alist (kwd-list-to-alist (cdr bits-spec)))
       ((assocs (low :low) (high :high) (width :width)) kwd-alist))
    (cond (width width)
          ((and (natp low) (natp high)) (1+ (- high low)))
          (t nil))))

(defun bits-spec-to-binder (bits-spec bound-var)
  (let ((var-name (car bits-spec))
        (part-select-args (cdr bits-spec))
        (bits-spec-width (calculate-bits-spec-width bits-spec)))
    (list 
     (if bits-spec-width
         ;; If we can determine the bitwidth of the result, generate a
         ;; `the` form to assist in reasoning about this value.
         `(the (unsigned-byte ,(calculate-bits-spec-width bits-spec)) ,var-name)
       var-name)
     `(bitops::part-select ,bound-var ,@part-select-args))))

(defun bits-specs-to-binders (bits-specs bound-var)
  (and (consp bits-specs)
       (cons (bits-spec-to-binder (car bits-specs) bound-var)
             (bits-specs-to-binders (cdr bits-specs) bound-var))))
  
(def-b*-binder bits
  :short "@(see b*) binder for splitting integers into bits"
  :decls ((declare (xargs :guard (destructure-guard bits args forms nil))))
  :body
  (let* ((binding (car forms))
         (evaledp (or (atom binding)
                      (eq (car binding) 'quote)))
         (form (if evaledp binding (pack binding)))
         (binders (bits-specs-to-binders args form)))
    (if evaledp
        `(b* ,binders ,rest-expr)
      `(let ((,form ,binding))
         (declare (ignorable ,form))
         (b* ,binders
           (check-vars-not-free (,form) ,rest-expr))))))

(local
 (assert!
  (equal (b* (((bits (ds :low 0 :width 16)
                     (cs :low 16 :width 16)
                     (bs :low 32 :width 16)
                     (as :low 48 :width 16)
                     (ds2 :low 0 :high 15)
                     (cs2 :low 16 :high 31)
                     (bs2 :low 32 :high 47)
                     (as2 :low 48 :high 63))
               #ux_AAAA_BBBB_CCCC_DDDD))
           (list ds cs bs as ds2 cs2 bs2 as2))
         '(#ux_DDDD #ux_CCCC #ux_BBBB #ux_AAAA
                    #ux_DDDD #ux_CCCC #ux_BBBB #ux_AAAA))))
