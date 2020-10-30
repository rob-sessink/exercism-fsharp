module BankAccount

// based upon http://www.fssnip.net/nQ/title/Agent-demo

type AccountAction =
    | Open of decimal
    | Update of decimal
    | GetBalance of AsyncReplyChannel<decimal option>
    | Close

type BankAccount() =

    let mutable active = false

    static let updateBalance current change =
        let newBalance = current + change
        newBalance

    let agent =
        MailboxProcessor.Start(fun inbox ->

            let rec loop current =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Open amount ->
                        active <- true
                        return! loop (updateBalance current amount)
                    | Update amount ->
                        if active
                        then return! loop (updateBalance current amount)
                        else return! loop current
                    | GetBalance repl ->
                        if active then repl.Reply(Some current) else repl.Reply(None)
                        return! loop current
                    | Close ->
                        active <- false
                        return! loop current
                }

            loop 0.0m)

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
