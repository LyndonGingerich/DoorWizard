namespace Library

type Vector =
    { X: int
      Y: int }

    static member op_Addition(lhs, rhs) =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member op_Subtraction(lhs, rhs) =
        { X = lhs.X - rhs.X; Y = lhs.Y - rhs.Y }

    static member op_Multiply(lhs: Vector, scale: int) =
        { X = lhs.X * scale; Y = lhs.Y * scale }

    static member op_LessThanOrEqual(lhs, rhs) = lhs.X <= rhs.X && lhs.Y <= rhs.Y

    static member op_Equals(lhs, rhs) = lhs.X = rhs.X && lhs.Y = rhs.Y

    static member op_LessThan(lhs, rhs) = lhs.X < rhs.X && lhs.Y < rhs.Y

    static member op_GreaterThanOrEqual(lhs, rhs) = lhs.X >= rhs.X && lhs.Y >= rhs.Y

    static member op_GreaterThan(lhs, rhs) = lhs.X > rhs.X && lhs.Y > rhs.Y

module Vector =
    let create x y = { X = x; Y = y }

    let abs location =
        create (abs location.X) (abs location.Y)

    let distanceFrom target location =
        let diff = abs (target - location)
        let sqr x = x * x
        let distanceSq = (sqr diff.X) + (sqr diff.Y)
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
