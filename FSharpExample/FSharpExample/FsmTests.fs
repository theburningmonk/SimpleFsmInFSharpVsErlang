module Fsm.Tests

open System
open System.Threading
open Fsm
open NUnit.Framework
open FsUnit

let getLock () = new CodeLock([1; 2; 3; 4])
let getOpenLock () = 
    let codelock = getLock()
    codelock.Button 1
    codelock.Button 2
    codelock.Button 3
    codelock.Button 4
    codelock

[<TestFixture>]
type ``Given a locked FSM`` () =   
    [<Test>]
    member test.``when the user hasn't entered anything its state should be blank`` () =
        let codelock = getLock()
        codelock.GetState() |> should equal <| Locked([])

    [<Test>]
    member test.``when the user enters the first digit it remains locked`` () =
        let codelock = getLock()
        codelock.Button 1
        codelock.GetState() |> should equal <| Locked([1])

    [<Test>]
    member test.``when the user enters an incorrect code then the state is reset`` () =
        let codelock = getLock()
        codelock.Button 1
        codelock.Button 2
        codelock.Button 3
        codelock.Button 5
        codelock.GetState() |> should equal <| Locked([])

    [<Test>]
    member test.``when the user enters the correct code then the state is changed to open`` () =
        let codelock = getLock()
        codelock.Button 1
        codelock.Button 2
        codelock.Button 3
        codelock.Button 4
        codelock.GetState() |> should equal Open

    [<Test>]
    member test.``when the user calls Close it should terminate the FSM`` () =
        let codelock = getLock()
        codelock.Close()
        codelock.GetState() |> should equal Closed

[<TestFixture>]
type ``Given an open FSM`` () =
    [<Test>]
    member test.``when 3 seconds have passed it should revert to locked state`` () =
        let codelock = getOpenLock()

        // give it an half a second as safety buffer
        Thread.Sleep(TimeSpan.FromSeconds 3.5)

        codelock.GetState() |> should equal <| Locked([])

    [<Test>]
    member test.``when user calls Close it should terminate the FSM`` () =
        let codelock = getOpenLock()

        codelock.Close()
        codelock.GetState() |> should equal Closed