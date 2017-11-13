type  Radius  =  Float
type  Width   =  Float
type  Height  =  Float

data  Shape  =  Circle Radius
             |  Rect Width Height
             deriving (Eq, Ord, Show)

area :: Shape -> Float
area (Circle r)  =  pi * r^2
area (Rect w h)  =  w * h
