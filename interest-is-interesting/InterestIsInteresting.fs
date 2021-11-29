module InterestIsInteresting

let interestRate (balance: decimal) : single =
    match balance with
    | b when b < 0m -> 3.213f
    | b when b < 1000m -> 0.5f
    | b when b >= 1000m && b < 5000m -> 1.621f
    | b when b >= 5000m -> 2.475f
    | _ -> failwith $"Invalid balance: {balance}"

let interest (balance: decimal) : decimal =
    (decimal (interestRate balance) / 100m) * balance

let annualBalanceUpdate (balance: decimal) : decimal = balance + interest balance

let amountToDonate (balance: decimal) (taxFreePercentage: float) : int =
    if balance < 0m then
        0
    else
        balance
        * (decimal (taxFreePercentage) / 100m)
        * 2m
        |> int
