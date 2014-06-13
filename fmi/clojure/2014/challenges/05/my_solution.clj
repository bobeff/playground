(defmacro lazy
  ([] `(lazy-seq ()))
  ([head & tail]
    `(lazy-seq
      (cons ~head
            (lazy ~@tail)))))
