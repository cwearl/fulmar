(let* ([c concat-chunk]
       [l literal-chunk]
       [n new-line-chunk]
       [e empty-chunk]
       [t1 (c (c e e)
              (c e e))]
       [t2 (c t1 t1)]
       [t3 (c t2 t2)]
       [t4 (c t3 t3)]
       [t5 (c t4 t4)]
       [t6 (c t5 t5)]
       [t7 (c t6 t6)]
       [t8 (c t7 t7)]
       [t9 (c t8 t8)]
       [t10 (c t9 t9)]
       [t11 (c t10 t10)]
       [t12 (c t11 t11)])
  t6)
