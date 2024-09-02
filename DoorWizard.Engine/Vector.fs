namespace DoorWizard.Engine

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

[<RequireQualifiedAccess>]
module Vector =
    let create x y = { X = x; Y = y }

    let abs location =
        create (abs location.X) (abs location.Y)

    let distanceFrom target location =
        let diff = abs (target - location)
        let sqr x = x * x
        let distanceSq = (sqr diff.X) + (sqr diff.Y)
        sqrt (float distanceSq)

module Direction =
    let idle = Vector.create 0 0
    let north = Vector.create 0 1
    let northEast = Vector.create 1 1
    let east = Vector.create 1 0
    let southEast = Vector.create 1 -1
    let south = Vector.create 0 -1
    let southWest = Vector.create -1 -1
    let west = Vector.create -1 0
    let northWest = Vector.create -1 1
