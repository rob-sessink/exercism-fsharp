module ValentinesDay

type Approval =
    | No
    | Yes
    | Maybe

type Cuisine =
    | Korean
    | Turkish

type Genre =
    | Crime
    | Horror
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity) : Approval =
    match activity with
    | BoardGame -> Approval.No
    | Chill -> Approval.No
    | Movie g ->
        match g with
        | Romance -> Approval.Yes
        | _ -> Approval.No
    | Restaurant c ->
        match c with
        | Korean -> Approval.Yes
        | Turkish -> Approval.Maybe
    | Walk i ->
        match i with
        | i when i < 3 -> Approval.Yes
        | i when i < 5 -> Approval.Maybe
        | _ -> Approval.No
