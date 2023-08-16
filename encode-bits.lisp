(in-package "ACL2")

(include-book "std/testing/assert-bang" :dir :system)
(include-book "centaur/bitops/part-install" :dir :system)

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
