module Vector

type Vector =
    { x: int
      y: int }

    static member op_Addition(lhs, rhs) =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member op_Subtraction(lhs, rhs) =
        { x = lhs.x - rhs.x; y = lhs.y - rhs.y }

    static member op_Multiply(lhs: Vector, scale: int) =
        { x = lhs.x * scale; y = lhs.y * scale }

    static member op_LessThanOrEqual(lhs, rhs) = lhs.x <= rhs.x && lhs.y <= rhs.y

    static member op_Equals(lhs, rhs) = lhs.x = rhs.x && lhs.y = rhs.y

    static member op_LessThan(lhs, rhs) = lhs.x < rhs.x && lhs.y < rhs.y

    static member op_GreaterThanOrEqual(lhs, rhs) = lhs.x >= rhs.x && lhs.y >= rhs.y

    static member op_GreaterThan(lhs, rhs) = lhs.x > rhs.x && lhs.y > rhs.y

let create x y = { x = x; y = y }

let abs location =
    create (abs location.x) (abs location.y)

let distanceFrom target location =
    let diff = abs (target - location)
    let sqr x = x * x
    let distanceSq = (sqr diff.x) + (sqr diff.y)
    sqrt (float distanceSq)

module Directions =
    let idle = create 0 0
    let north = create 0 1
    let northEast = create 1 1
    let east = create 1 0
    let southEast = create 1 -1
    let south = create 0 -1
    let southWest = create -1 -1
    let west = create -1 0
    let northWest = create -1 1
