
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

    type Message<'a> =
        | FindUser of UUID * (User option->'a)
        | UpdateUser of User * (unit->'a)
        | SendEmail of email:string * subject:string * body:string * (unit->'a)

    type Batch<'a> =
        | Pure of 'a
        | Free of Message<Batch<'a>>

    let inline private findUser uuid = FindUser (uuid, Pure) |> Free
    let inline private updateUser user = UpdateUser (user, Pure) |> Free
    let inline private sendEmail x y z = SendEmail (x,y,z, Pure) |> Free

    let inline private map f = function
        | FindUser (x, next) -> FindUser (x, next >> f)
        | UpdateUser (x, next) -> UpdateUser (x, next >> f)
        | SendEmail (x,y,z, next) -> SendEmail (x,y,z, next >> f)

    let rec inline private bind f = function
        | Pure x -> f x
        | Free x -> map (bind f) x |> Free

    type BatchBuilder() =
        member inline __.Bind (x, f) = bind f x
        member inline __.Return x = Pure x
        member inline __.ReturnFrom x = x
        member inline __.Zero () = Pure ()

    let batch = BatchBuilder()

    let addPoints userId pointsToAdd = batch {
        let! u = findUser userId
        match u with
        | None -> return Some "User not found"
        | Some user ->
            let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
            do! updateUser updated
            do! sendEmail user.email "Points added!" ("You now have " + string updated.loyaltyPoints)
            return None
    }

    let getPoints userId = batch {
        let! u = findUser userId
        return u |> Option.map (fun u -> u.loyaltyPoints)
    }

    type Interpreter =
        abstract member Run : Batch<'a> -> 'a

module EmailerModule =

    type Batch<'a> =
        | X

    let sendEmail (email:string) (subject:string) (body:string) = Batch<unit>.X

    let sendEmailAsync (email:string) (subject:string) (body:string) = async {
        return Batch<unit>.X
    }

module EmailerInterpreter =

    open EmailerModule
    let rec interpret (p:Batch<'a>) : 'a =
        failwith "hi"

    let rec interpretAsync (p:Batch<'a>) : Async<'a> =
        failwith "hi"

module Test =

    open LoyaltyPointsFree

    let rec interpret p =
        let run x =
            match x with
            | FindUser (x, next) -> Some {id=x; email="a@b.c"; loyaltyPoints=0} |> next
            | UpdateUser (_, next) -> () |> next
            | SendEmail (email,subject,body, next) ->
                EmailerModule.sendEmail email subject body
                |> EmailerInterpreter.interpret
                |> next
        match p with
        | Pure x -> x
        | Free x -> run x |> interpret

    let rec interpretAsync p = async {
        let inline run x = async {
            match x with
            | FindUser (x, next) -> return Some {id=x; email="a@b.c"; loyaltyPoints=0} |> next
            | UpdateUser (_, next) -> return next()
            | SendEmail (email,subject,body, next) ->
                let! message = EmailerModule.sendEmailAsync email subject body
                do! EmailerInterpreter.interpretAsync message
                return next()
        }
        match p with
        | Pure x -> return x
        | Free x ->
            let! result = run x
            return! interpretAsync result
    }


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

//     let tester = postbox |> Message.postAndReply (fun reply -> DoSomething (10, reply))


[<EntryPoint>]
let main _argv =
    0