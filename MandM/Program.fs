
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

        let batch = BatchBuilder()

    module Incoming =

        type Message<'a> =
            | AddPoints of UUID * int * (string option -> 'a)
            | GetPoints of UUID * (int option -> 'a)

        type Batch<'a> =
            | Pure of 'a
            | Free of Message<Batch<'a>>

        let inline addPoints u i = AddPoints (u,i, Pure) |> Free
        let inline getPoints u = GetPoints (u, Pure) |> Free

        let inline map f = function
            | AddPoints (u,i, next) -> AddPoints (u,i, next >> f)
            | GetPoints (u, next) -> GetPoints (u, next >> f)

        let rec inline bind f = function
            | Pure x -> f x
            | Free x -> map (bind f) x |> Free

        type BatchBuilder() =
            member inline __.Bind (x, f) = bind f x
            member inline __.Return x = Pure x
            member inline __.ReturnFrom x = x
            member inline __.Zero () = Pure ()

        let private batch = BatchBuilder()


    let addPoints userId pointsToAdd = Outgoing.batch {
        let! u = Outgoing.findUser userId
        match u with
        | None -> return Some "User not found"
        | Some user ->
            let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
            do! Outgoing.updateUser updated
            do! Outgoing.sendEmail user.email "Points added!" ("You now have " + string updated.loyaltyPoints)
            return None
    }

    let getPoints userId = Outgoing.batch {
        let! u = Outgoing.findUser userId
        return u |> Option.map (fun u -> u.loyaltyPoints)
    }

    let outgoingInterpreter (b:Outgoing.Batch<'a>) : 'a =
        failwith "hi"

    let run = function
        | Incoming.AddPoints (u,i, next) -> addPoints u i |> outgoingInterpreter |> next
        | Incoming.GetPoints (u, next) -> getPoints u |> outgoingInterpreter |> next

    let rec private incomingInterpret (batch:Incoming.Batch<'b>) =
        match batch with
        | Incoming.Pure x -> x
        | Incoming.Free x -> run x |> incomingInterpret

    let post (batch:Incoming.Batch<'b>) =
        incomingInterpret batch


// module Message =
//     open System.Threading

//     let wait (f : ('a -> unit) -> unit) =
//         use mre = new ManualResetEventSlim(false)
//         let mutable v = Unchecked.defaultof<_>
//         let reply a =
//             v <- a
//             mre.Set()
//         f reply
//         mre.Wait()
//         v

//     let waitAsync (f : ('a -> unit) -> unit) = async {
//         use mre = new ManualResetEventSlim(false)
//         let mutable v = Unchecked.defaultof<_>
//         let reply a =
//             v <- a
//             mre.Set()
//         f reply
//         let! _ = Async.AwaitWaitHandle mre.WaitHandle
//         return v
//     }

//     let postAndReply (f : ('Reply -> unit) -> 'Message) postBox =
//         wait (f >> postBox)

//     let postAndAsyncReply (f : ('Reply -> unit) -> 'Message) postBox =
//         waitAsync (f >> postBox)


// module Test =

//     open LoyaltyPointsFree
//     open MessageWork


//     let run = function
//         | FindUser (x, next) -> Some {id=x; email="a@b.c"; loyaltyPoints=0} |> next
//         | UpdateUser (_, next) -> () |> next
//         | SendEmail (_,_,_, next) -> () |> next

//     let rec interpret p =
//         match p with
//         | Pure x -> x
//         | Free x -> run x |> interpret

//     let tester = postbox |> Message.postAndReply (fun reply -> DoSomething (10, reply))


[<EntryPoint>]
let main _argv =
    0