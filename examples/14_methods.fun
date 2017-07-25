(package main

(struct rect
  (width  int)
  (height int)
)

(method (r (:ptr :rect)) area () :int
  (* r.width r.height)
)

(method (r :rect) perim () :int
  (+ (* 2 r.width) (* 2 r.height))
)

(func main () (
  (var r (:rect ((width 10) (height 5))))
  (print "area: " (r.area))
  (print "perim: " (r.perim))
  (var rp (& r))
  (print "area: " (rp.area))
  (print "perim:" (rp.perim))
))
)
