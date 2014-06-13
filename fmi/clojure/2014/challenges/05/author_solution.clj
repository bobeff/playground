(defmacro lazy
  [& exprs]
  `(map force
     (list ~@(for [expr exprs]
               `(delay ~expr)))))
