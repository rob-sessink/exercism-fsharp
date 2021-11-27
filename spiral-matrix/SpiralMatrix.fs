module SpiralMatrix

type Traverse =
    | Left
    | Right
    | Up
    | Down

type Index =
    { top: int
      bottom: int
      left: int
      right: int
      value: int
      traverse: Traverse }

let spiralMatrix size =

    let m = Array2D.init size size (fun _ _ -> 0)

    let ptr =
        { top = 0
          bottom = size - 1
          left = 0
          right = size - 1
          value = 0
          traverse = Traverse.Right }

    // fill by function:f, using sequence: seqs and initial value: v
    let fillFold v seqs f = (v, seqs) ||> Seq.fold f

    // set matrix with value and increment
    let setAndIncrement row col v (m: int [,]) =
        m.[row, col] <- v + 1
        v + 1

    let populateRight ptr (m: int [,]) =
        // get traversal sequence
        let seqs =
            seq { for i in ptr.top .. 1 .. ptr.bottom -> i }

        // define filling function
        let f =
            (fun v i -> setAndIncrement ptr.top i v m)

        { ptr with
              value = (fillFold ptr.value seqs f) // fold filling
              top = ptr.top + 1
              traverse = Traverse.Down }

    let populateDown ptr (m: int [,]) =
        let seqs =
            seq { for i in ptr.top .. 1 .. ptr.bottom -> i }

        let f =
            (fun v i -> setAndIncrement i ptr.right v m)

        { ptr with
              value = (fillFold ptr.value seqs f)
              right = ptr.right - 1
              traverse = Traverse.Left }

    let populateLeft ptr (m: int [,]) =
        let seqs =
            seq { for i in ptr.right .. -1 .. ptr.left -> i }

        let f =
            (fun v i -> setAndIncrement ptr.bottom i v m)

        { ptr with
              value = (fillFold ptr.value seqs f)
              bottom = ptr.bottom - 1
              traverse = Traverse.Up }

    let populateUp ptr (m: int [,]) =
        let seqs =
            seq { for i in ptr.bottom .. -1 .. ptr.top -> i }

        let f =
            (fun v i -> setAndIncrement i ptr.left v m)

        { ptr with
              value = (fillFold ptr.value seqs f)
              left = ptr.left + 1
              traverse = Traverse.Right }

    let toLoL (m: int [,]) =
        [ let height = m.GetLength 0

          for row in 0 .. height - 1 do
              yield m.[row, *] |> List.ofArray ]


    let rec traverse ptr m =
        if (ptr.top <= ptr.bottom && ptr.left <= ptr.right) then
            let ptr_new =
                match ptr.traverse with
                | Left -> populateLeft ptr m
                | Down -> populateDown ptr m
                | Right -> populateRight ptr m
                | Up -> populateUp ptr m

            traverse ptr_new m
        else
            m |> toLoL

    traverse ptr m
