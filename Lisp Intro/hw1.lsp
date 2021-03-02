
;This function PAD takes a single integer argument N,
; and returns the Nth Padovan number
(defun PAD (n)
  ;Base case: when n = 0,1,2 return 1
  (cond ((= n 0) 1)
        ((= n 1) 1)
        ((= n 2) 1)
        ; recursion using the formula PAD(N) = PAD(N-2) + PAD(N-3)
        (t (+ (PAD (- n 2)) (PAD (- n 3))))
  )
)

; This function SUMS takes a single numeric argument N,
; and returns the number of additions required by the above PAD function
; to compute the Nth Padovan number.
(defun SUMS (N)
  ;Base case: when n=0,1,2 return 0, since it needs 0 step to compute the sum
  (cond ((= N 0) 0)
        ((= N 1) 0)
        ((= N 2) 0)
        ; recursion: the number of steps needed is the steps needed to compute
        ; SUMS(N-2) + steps needed to compute SUMS(N-3) + 1 (for this step)
        (t (+ (SUMS (- n 2)) (+ (SUMS (- n 3)) 1)))
  )
)

; This function ANON takes a single argument TREE that represents a tree,
; and returns an anonymized tree with the same structure,
; but where all symbols and numbers in the tree are replaced by a "?"
(defun ANON (TREE)
  ; if there is no node, return nil
  (cond ((not TREE) nil)
        ((atom TREE) (append '?)) ;If only contains a single character, append ? to the list
        ; recursion: decompose the tree into first and last component, compute ANON separately,
        ; and incorperate the two resulting list together
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
  )
)
