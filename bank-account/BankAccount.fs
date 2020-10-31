module BankAccount

// based upon
// http://www.fssnip.net/nQ/title/Agent-demo
// https://www.codemag.com/article/1707051/Writing-Concurrent-Programs-Using-F

type Status =
    | Open
    | Closed

type Account = { Balance: decimal; Status: Status }

type AccountAction =
    | Open of decimal
    | Update of decimal
    | GetBalance of AsyncReplyChannel<decimal option>
    | Close

type BankAccount() =

    let initialize =
        { Balance = 0.0m
          Status = Status.Closed }

    let updateBalance change account =
        match account.Status with
        | Status.Open ->
            { account with
                  Balance = account.Balance + change }
        | Status.Closed -> account

    let openAccount amount =
        { Balance = amount
          Status = Status.Open }


    let getBalance account =
        match account.Status with
        | Status.Open -> Some account.Balance
        | Status.Closed -> None

    let closeAccount account = { account with Status = Status.Closed }

    let agent =
        MailboxProcessor<AccountAction>
            .Start(fun inbox ->

                  let rec loop current =
                      async {
                          let! msg = inbox.Receive()

                          let balance =
                              match msg with
                              | Open amount -> openAccount amount
                              | Update amount -> updateBalance amount current
                              | GetBalance repl ->
                                  repl.Reply(getBalance current)
                                  current
                              | Close -> closeAccount current

                          return! loop balance
                      }

                  loop initialize)

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
