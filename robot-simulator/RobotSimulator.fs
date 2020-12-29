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

    let turn movement robot =
        let turned =
            match movement with
            | Left when robot.direction = North -> West
            | Left when robot.direction = West -> South
            | Left when robot.direction = South -> East
            | Left when robot.direction = East -> North
            | Right when robot.direction = North -> East
            | Right when robot.direction = East -> South
            | Right when robot.direction = South -> West
            | Right when robot.direction = West -> North
            | _ -> robot.direction

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

    let asMovement letter =
        match letter with
        | 'L' -> Left
        | 'R' -> Right
        | 'A' -> Advance
        | _ -> Hold

    let move robot instruction =
        match asMovement instruction with
        | Left -> turn Left robot
        | Right -> turn Right robot
        | Advance -> advance robot
        | Hold -> robot

    instructions |> Seq.fold move robot
