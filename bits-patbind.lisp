(in-package "ACL2")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "centaur/bitops/part-select" :dir :system)
(include-book "util")

;; Take a "bits-spec" and turn it into a binding
;; A bits-spec is a list consisting of a symbol followed by keyword arguments
;; that part-select understands.
;; We call the part of the list after the symbol the "part-select spec"

;; Calculate the constant width of a bit-spec, if possible
(defun calculate-bits-spec-width (bits-spec)
  (b* ((kwd-alist (kwd-list-to-alist (cdr bits-spec)))
       ((assocs (low :low) (high :high) (width :width)) kwd-alist))
    (cond ((and width (natp width)) width)
          ((and (natp low) (natp high))
           (1+ (if (<= low high)
                   (- high low)
                 (- low high))))
          (t nil))))

;; Turn a bit-spec into a part-select (or logrev of a part-select) as appropriate
;; Disabling the ability to write out-of-order :high and :low for now
#|
(defun part-select-spec-to-part-select (part-select-spec val)
  (b* ((part-select-spec-alist (kwd-list-to-alist part-select-spec))
       ((assocs (low :low) (high :high)) part-select-spec-alist)
       (part-select-expr `(bitops::part-select ,val ,@part-select-spec)))
    ;; minor optimization: don't generate if statement if we can tell
    ;; it's not needed
    (if (not (and low high))
        part-select-expr
      (let ((rev-part-select-expr `(bitops::logrev (1+ (- ,low ,high)) (bitops::part-select ,val :low ,high :high ,low))))
        (cond
         ((equal low 0) part-select-expr)
         ((not (and (natp low) (natp high)))
          `(if (> ,low ,high) ,rev-part-select-expr ,part-select-expr))
         ((> low high) rev-part-select-expr)
         (t part-select-expr))))))
|#

(defun part-select-spec-to-part-select (part-select-spec val)
  `(bitops::part-select ,val ,@part-select-spec))

(defun bits-spec-to-binder (bits-spec bound-var)
  (b* ((var-name (car bits-spec))
       (bits-spec-width (calculate-bits-spec-width bits-spec)))
    (list
     (if bits-spec-width
         ;; If we can determine the bitwidth of the result, generate a
         ;; `the` form to assist in reasoning about this value.
         `(the (unsigned-byte ,bits-spec-width) ,var-name)
       var-name)
     (part-select-spec-to-part-select (cdr bits-spec) bound-var))))

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
                     ;; select in reverse
                     ;;(ab-rev :low 23 :high 8))
               #ux_AAAA_BBBB_CCCC_DDDD))
           (list ds cs bs as ds2 cs2 bs2 as2));; ab-rev))
         '(#ux_DDDD #ux_CCCC #ux_BBBB #ux_AAAA
                    #ux_DDDD #ux_CCCC #ux_BBBB #ux_AAAA))))
                    ;; 0xDDCC reversed is 0xBB33
                    ;;#ux_BB33))))
