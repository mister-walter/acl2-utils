(in-package "ACL2")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "std/testing/must-succeed" :dir :system)
(include-book "centaur/bitops/part-install" :dir :system)
(include-book "util")
(include-book "bits-patbind")

;; This describes setting some bit range in a value (described by the
;; cdr) to another value (the car)
;; e.g. the bits-spec (x :low 3 :width 5) represents setting the bits
;; 3 to 8 to the low 5 bits of the variable x
;; A spec must consist of :low and one of :high or :width.
;; See part-install for more information about these arguments.
(defun encode-bits-fn (init-val specs)
  (if (endp specs)
      init-val
    `(bitops::part-install ,(car (car specs))
                           ,(encode-bits-fn init-val (cdr specs))
                           ,@(cdr (car specs)))))

;; This is sort of the inverse of the bits b* binder above
;; Basically, you start with the given init-val (the first argument)
;; and sequentially set different ranges of the val to the given values described
;; in specs.
(defmacro encode-bits (init-val &rest specs)
  (encode-bits-fn init-val (reverse specs)))

(local
 (assert!
  (equal (encode-bits 1
                      (1 :low 1 :high 1)
                      (7 :low 2 :width 2)
                      ((+ 1 1) :low 4 :high 5))
         #b101111)))
                      
;; Overlapping ranges are OK, and will be resolved similarly to a let
;; (e.g. applied in order)
(local
 (assert!
  (equal (encode-bits 0
                      (#b110000 :low 0 :high 5)
                      (#b0011   :low 1 :high 4)
                      (#b00     :low 0 :width 2))
         #b100100)))

(defun concat-bits-spec-to-val (spec)
  (b* ((spec-val (car spec))
       (spec-alist (kwd-list-to-alist (cdr spec)))
       ((assocs (low :low) (high :high) (width :width)) spec-alist)
       (low (if (and (not low) (not high) width) 0 low))
       (new-spec `(,@(and low `(:low ,low)) ,@(and high `(:high ,high)) ,@(and width `(:width ,width)))))
    (part-select-spec-to-part-select new-spec spec-val)))

(defun concat-bits-spec-width (spec)
  (b* ((spec-alist (kwd-list-to-alist (cdr spec)))
       ((assocs (low :low) (high :high) (width :width)) spec-alist))
    (cond (width width)
          ((and (natp low) (natp high)) (1+ (abs (- high low))))
          ((and low high) `(1+ (abs (- ,high ,low))))
          (t (er hard 'concat-bits "You must provide either :width (and optionally :low) or both :low and :high to concat-bits")))))

;; See the concat-bits macro below
(defun concat-bits-fn (specs acc-width acc-expr)
  (if (endp specs)
      acc-expr
    (concat-bits-fn (cdr specs)
                    `(+ ,(concat-bits-spec-width (car specs)) ,acc-width)
                    `(bitops::logapp ,acc-width ,acc-expr ,(concat-bits-spec-to-val (car specs))))))

;; (concat-bits (x :width 4) ((+ 1 2) :width 1) (3 :width (/ 4 2)))
;; Each spec is a list (<val> :width <width-val>)
;; Both <val> and <width-val> can be expressions
;; Specs are listed in order from MSB to LSB. So,
;; (concat-bits (x :width 2) (y :width 3)) will produce
;; a value V consisting of the 3 low bits of y in V's 3 low bits
;; and the 2 low bits of x in V's bits 3 and 4.
(defmacro concat-bits (&rest specs)
  (concat-bits-fn (reverse specs) 0 0))

(local
 (assert!
  (equal (concat-bits (#b10 :width 2) (#b11 :width 1) (#b1 :width 3))
         #b101001)))

(local (include-book "centaur/bitops/ihsext-basics" :dir :system))
(must-succeed
  (thm (implies (and (integerp a)
                     (integerp b)
                     (>= b 0)
                     (posp w-b))
                (equal (concat-bits (a :width 0) (b :width w-b))
                       (bitops::loghead w-b b)))))
