﻿
type UUID = int
type User = {id:UUID; email:string; loyaltyPoints:int}

module LoyaltyPoints =

    type IContext =
        abstract member FindUser : UUID -> Async<User option>
        abstract member UpdateUser : User -> Async<unit>
        abstract member SendEmail : email:string * subject:string * body:string -> Async<unit>

    let addPoints (context:IContext) userId pointsToAdd = async {
        let! u = context.FindUser userId
        match u with
        | None -> return Some "User not found"
        | Some user ->
            let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
            do! context.UpdateUser updated
            do! context.SendEmail(user.email, "Points added!", "You now have " + string updated.loyaltyPoints)
            return None
    }

module LoyaltyPointsFree =

    module Outgoing =

        type Message<'a> =
            | FindUser of UUID * (User option->'a)
            | UpdateUser of User * (unit->'a)
            | SendEmail of email:string * subject:string * body:string * (unit->'a)

        type Batch<'a> =
            | Pure of 'a
            | Free of Message<Batch<'a>>

        let inline findUser uuid = FindUser (uuid, Pure) |> Free
        let inline updateUser user = UpdateUser (user, Pure) |> Free
        let inline sendEmail x y z = SendEmail (x,y,z, Pure) |> Free

        let inline map f = function
            | FindUser (x, next) -> FindUser (x, next >> f)
            | UpdateUser (x, next) -> UpdateUser (x, next >> f)
            | SendEmail (x,y,z, next) -> SendEmail (x,y,z, next >> f)

        let rec inline bind f = function
            | Pure x -> f x
            | Free x -> map (bind f) x |> Free

        type BatchBuilder() =
            member inline __.Bind (x, f) = bind f x
            member inline __.Return x = Pure x
            member inline __.ReturnFrom x = x
            member inline __.Zero () = Pure ()

        let command = BatchBuilder()


    let addPoints userId pointsToAdd = Outgoing.command {
        let! u = Outgoing.findUser userId
        match u with
        | None -> return Some "User not found"
        | Some user ->
            let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
            do! Outgoing.updateUser updated
            do! Outgoing.sendEmail user.email "Points added!" ("You now have " + string updated.loyaltyPoints)
            return None
    }

module Message =
    open System.Threading

    let wait (f : ('a -> unit) -> unit) =
        use mre = new ManualResetEventSlim(false)
        let mutable v = Unchecked.defaultof<_>
        let reply a =
            v <- a
            mre.Set()
        f reply
        mre.Wait()
        v

    let waitAsync (f : ('a -> unit) -> unit) = async {
        use mre = new ManualResetEventSlim(false)
        let mutable v = Unchecked.defaultof<_>
        let reply a =
            v <- a
            mre.Set()
        f reply
        let! _ = Async.AwaitWaitHandle mre.WaitHandle
        return v
    }

    let postAndReply (f : ('Reply -> unit) -> 'Message) postBox =
        wait (f >> postBox)

    let postAndAsyncReply (f : ('Reply -> unit) -> 'Message) postBox =
        waitAsync (f >> postBox)

module MessageWork =
    
    type Message<'a> =
        | DoSomething of int * (string -> 'a)
        | DoSomething2 of string * (int -> 'a)

    type MessageBatch<'a> =
        | Pure of 'a
        | Free of Message<MessageBatch<'a>>

    let inline private doSomething i = DoSomething (i, Pure) |> Free
    let inline private doSomething2 s = DoSomething2 (s, Pure) |> Free

    let inline private map f = function
        | DoSomething (x, next) -> DoSomething (x, next >> f)
        | DoSomething2 (x, next) -> DoSomething2 (x, next >> f)

    let rec inline private bind f = function
        | Pure x -> f x
        | Free x -> map (bind f) x |> Free

    type private MessageBatchBuilder() =
        member inline __.Bind (x, f) = bind f x
        member inline __.Return x = Pure x
        member inline __.ReturnFrom x = x
        member inline __.Zero () = Pure ()

    let private messageBatch = MessageBatchBuilder()

    let postbox (_:MessageBatch<'a>) = ()

module Test =

    open LoyaltyPointsFree
    open MessageWork


    let run = function
        | FindUser (x, next) -> Some {id=x; email="a@b.c"; loyaltyPoints=0} |> next
        | UpdateUser (_, next) -> () |> next
        | SendEmail (_,_,_, next) -> () |> next

    let rec interpret p =
        match p with
        | Pure x -> x
        | Free x -> run x |> interpret

    let tester = postbox |> Message.postAndReply (fun reply -> DoSomething (10, reply))


[<EntryPoint>]
let main _argv =
    0