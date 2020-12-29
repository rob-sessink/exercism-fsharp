module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Movement =
    | Left
    | Right
    | Advance
    | Hold

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let create direction position =
    { direction = direction
      position = position }

let move instructions robot =

    let turn robot movement =
        let turned =
            match robot.direction, movement with
            | North, Left -> West
            | West, Left -> South
            | South, Left -> East
            | East, Left -> North
            | North, Right -> East
            | East, Right -> South
            | South, Right -> West
            | West, Right -> North
            | direction, _ -> direction

        { robot with direction = turned }

    let advance robot =
        let (x, y) = robot.position

        let advanced =
            match robot.direction with
            | North -> (x, y + 1)
            | East -> (x + 1, y)
            | South -> (x, y - 1)
            | West -> (x - 1, y)

        { robot with position = advanced }

    let hold robot = robot

    let asMovement letter =
        match letter with
        | 'L' -> Left
        | 'R' -> Right
        | 'A' -> Advance
        | _ -> Hold

    let move robot instruction =
        match asMovement instruction with
        | Left -> turn robot Left 
        | Right -> turn robot Right
        | Advance -> advance robot
        | Hold -> hold robot

    instructions |> Seq.fold move robot
