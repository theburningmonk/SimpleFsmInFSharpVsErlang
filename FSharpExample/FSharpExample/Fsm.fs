namespace Fsm

open System
open System.Threading
open FSharp.Control

type State = | Locked of int list
             | Open
             | Closed

type Message = | Button     of int
               | Close      of AsyncReplyChannel<unit>
               | GetState   of AsyncReplyChannel<State>

type CodeLock (code : int list) =
    let code = code |> List.rev
    let doUnlock () = printfn "The code lock is now unlocked"
    let doLock () = printfn "The code lock is now locked"
    let doCleanUp () = printfn "The code lock is now terminated..."

    let cts = new CancellationTokenSource()
    let agent = Agent<Message>.Start((fun inbox ->
        let rec lockedState (soFar : int list) = 
            async {
                let! msg = inbox.Receive()
                match msg with
                | Button(digit) when soFar.Length < code.Length - 1 -> return! lockedState (digit::soFar)
                | Button(digit) when (digit::soFar) = code -> doUnlock()
                                                              return! openState ()
                | Button(digit) -> return! lockedState []
                | GetState(reply) -> reply.Reply(Locked soFar)
                                     return! lockedState soFar
                | Close(reply) -> doCleanUp()
                                  reply.Reply()
                                  cts.Cancel()
                | _ -> return! lockedState soFar
            }
        and openState () =
            async {
                try
                    let! msg = inbox.Receive(3000)
                    match msg with
                    | GetState(reply)
                        -> reply.Reply(Open)
                           return! openState()
                    | Close(reply) 
                        -> doCleanUp()
                           reply.Reply()
                           cts.Cancel()
                    | _ -> return! openState()
                with
                | :? TimeoutException -> doLock()
                                         return! lockedState []                    
            }
            
        lockedState []), cancellationToken = cts.Token)

    member this.Button digit = agent.Post <| Button(digit)
    member this.GetState () = try agent.PostAndReply(GetState, 1000) with | :? TimeoutException -> Closed
    member this.Close () = agent.PostAndReply Close