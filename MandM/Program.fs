open System

type Reply<'a>(f : 'a -> unit) =
    member __.Reply(value) = f value

module Reply =
    let map f (r:Reply<'a>) =
        Reply(f >> r.Reply)

type IModule<'messageIn,'messageOut> =
    abstract member Post : 'messageIn -> unit
    abstract member PostAndReply : (Reply<'b> -> 'messageIn) -> 'b
    abstract Deliver : IObservable<'messageOut>

type Module<'messageIn,'messageOut> = {
    Update: ('messageOut -> unit) -> 'messageIn -> unit
}

module Module =
    let x = 1

type UUID = int
type User = {id:UUID; email:string; loyaltyPoints:int}

module LoyaltyPoints =

    type MessageIn =
        | AddPoints of userId: UUID * pointsToAdd: int * Reply<string option>

    type MessageOut =
        | FindUser of userId: UUID * Reply<User option>
        | UpdateUser of user: User
        | SendEmail of email: string * subject: string * body: string
    let update (command:MessageOut -> unit) (update:MessageIn) =
        match update with
        | AddPoints (userId,pointsToAdd,reply) ->
            FindUser (userId,
                reply |> Reply.map (function
                | None -> Some "User not found"
                | Some user ->
                    let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
                    UpdateUser updated |> command
                    SendEmail(user.email, "Points added!", sprintf "You now have %i" updated.loyaltyPoints) |> command
                    None)
            ) |> command

// case class User(id: UUID, email: String, loyaltyPoints: Int)

// trait EmailService {
//   def sendEmail(email: String, subject: String, body: String): Future[Unit]
// }

// trait UserRepository {
//   def findUser(id: UUID): Future[Option[User]]
//   def updateUser(u: User): Future[Unit]
// }

// class LoyaltyPoints(ur: UserRepository, es: EmailService) {
//   def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
//     ur.findUser(userId).flatMap {
//       case None => Future.successful(Left("User not found"))
//       case Some(user) =>
//         val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
//         for {
//           _ <- ur.updateUser(updated)
//           _ <- es.sendEmail(user.email, "Points added!", 
//                             s"You now have ${updated.loyaltyPoints}")
//         } yield Right(())
//     }
//   }
// }

[<EntryPoint>]
let main _argv =
    0
