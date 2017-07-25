(package main

(import "math")

(interface geometry
  (area  () :float64)
  (perim () :float64)
)

(struct rect
  (width  float64)
  (height float64)
)

(struct circle
  (radius float64)
)

(method (r :rect) area () :float64
  (* r.width r.height)
)

(method (r :rect) perim () :float64
  (+ (* 2 r.width) (* 2 r.height))
)

(method (c :circle) area () :float64
  (* math.Pi c.radius c.radius)
)

(method (c :circle) perim () :float64
  (* 2 math.Pi c.radius)
)

(func measure ((g :geometry)) (
  (print g)
  (print (g.area))
  (print (g.perim))
))

(func main (
  (var r (:rect ((width 3) (height 4))))
  (var c (:circle ((radius 5))))
  (measure r)
  (measure c)
))
)
