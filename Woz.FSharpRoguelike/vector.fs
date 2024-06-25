module Vector

type Vector =
    { x: int
      y: int }

    static member create x y = { x = x; y = y }

    static member op_Addition(lhs, rhs) =
        Vector.create (lhs.x + rhs.x) (lhs.y + rhs.y)

    static member op_Subtraction(lhs, rhs) =
        Vector.create (lhs.x - rhs.x) (lhs.y - rhs.y)

    static member op_Multiply(lhs: Vector, scale: int) =
        Vector.create (lhs.x * scale) (lhs.y * scale)

    static member op_LessThanOrEqual(lhs, rhs) = lhs.x <= rhs.x && lhs.y <= rhs.y

    static member op_Equals(lhs, rhs) = lhs.x = rhs.x && lhs.y = rhs.y

    static member op_LessThan(lhs, rhs) = lhs.x < rhs.x && lhs.y < rhs.y

    static member op_GreaterThanOrEqual(lhs, rhs) = lhs.x >= rhs.x && lhs.y >= rhs.y

    static member op_GreaterThan(lhs, rhs) = lhs.x > rhs.x && lhs.y > rhs.y

let abs location =
    Vector.create (abs location.x) (abs location.y)

let distanceFrom target location =
    let diff = abs (target - location)
    let sqr x = x * x
    let distanceSq = (sqr diff.x) + (sqr diff.y)
    sqrt (float distanceSq)

module Directions =
    let idle = Vector.create 0 0
    let north = Vector.create 0 1
    let northEast = Vector.create 1 1
    let east = Vector.create 1 0
    let southEast = Vector.create 1 (-1)
    let south = Vector.create 0 -1
    let southWest = Vector.create (-1) (-1)
    let west = Vector.create (-1) 0
    let northWest = Vector.create -1 1
