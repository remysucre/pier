#lang rosette/safe

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/synthax    ; provides `??`
         rosette/lib/destruct)  ; provipes `destruct`

(require "ops.rkt"
         "defs.rkt"
         "interp.rkt")

;; MIN_{y,w1,w2,w} w * R(x,y,w1) * E(y,x,w2) * 1_{w=w1*w2}
(define l
  (interpret
   (push-sum
    (simplify
     (push-sum
      (op-sum-int
       'y
       (op-sum
        'w1
        (op-sum
         'w2
         (op-sum
          'w
          (op-t* 'w
                 (op-t* (op-I (rel-R 'x 'y 'w1))
                        (op-t* (op-I (rel-E 'y 'x 'w2))
                               (op-I (op-eq? 'w (op-t* 'w1 'w2)))))))))))))))

;; MIN_{y,w1,w2} R(x,y,w1) * E(y,x,w2) * w1 * w2
(define r
  (interpret
   (simplify
    (push-sum
     (op-sum-int
      'y
      (op-sum
       'w1
       (op-sum
        'w2
        (op-t* (op-I (rel-R 'x 'y 'w1))
               (op-t* (op-I (rel-E 'y 'x 'w2))
                      (op-t* 'w1 'w2))))))))))

(verify (assert (eq? l r)))
