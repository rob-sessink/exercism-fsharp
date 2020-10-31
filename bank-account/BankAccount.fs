module BankAccount

// based upon http://www.fssnip.net/nQ/title/Agent-demo

type Status =
    | Open
    | Closed

type Account =
    { mutable Balance: decimal
      mutable Status: Status }

type AccountAction =
    | Open of decimal
    | Update of decimal
    | GetBalance of AsyncReplyChannel<decimal option>
    | Close

type BankAccount() =

    let init =
        { Balance = 0.0m
          Status = Status.Closed }
    
    let updateBalance change account =
        match account.Status with
        | Status.Open -> account.Balance <- account.Balance + change
        | Status.Closed -> ()

    let openAccount amount account =
        account.Balance <- amount
        account.Status <- Status.Open

    let getBalance account =
        match account.Status with
        | Status.Open -> Some account.Balance
        | Status.Closed -> None

    let closeAccount account = account.Status <- Status.Closed

    let agent =
        MailboxProcessor.Start(fun inbox ->

            let rec loop current =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Open amount -> current |> openAccount amount
                    | Update amount -> current |> updateBalance amount
                    | GetBalance repl -> repl.Reply(getBalance current)
                    | Close -> closeAccount current

                    printfn "Received msg: "
                    return! loop current
                }

            loop init)

    member this.Open =
        agent.Post(AccountAction.Open 0.0m)
        this

    member this.Update change =
        agent.Post(AccountAction.Update change)
        this

    member this.GetBalance =
        agent.PostAndReply(AccountAction.GetBalance)

    member this.Close =
        agent.Post(AccountAction.Close)
        this


let mkBankAccount () = BankAccount()

let openAccount (account: BankAccount) = account.Open

let closeAccount (account: BankAccount) = account.Close

let getBalance (account: BankAccount) = account.GetBalance

let updateBalance change (account: BankAccount) = account.Update change
