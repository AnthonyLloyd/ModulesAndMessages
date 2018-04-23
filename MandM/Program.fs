
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

    type Command<'a> =
        | FindUser of UUID * (User option->'a)
        | UpdateUser of User * (unit->'a)
        | SendEmail of email:string * subject:string * body:string * (unit->'a)

    type Program<'a> =
        | Pure of 'a
        | Free of Command<Program<'a>>

    let rec inline private bind f = function
        | Pure x -> f x
        | Free (FindUser (x, next)) -> FindUser (x, next >> bind f) |> Free
        | Free (UpdateUser (x, next)) -> UpdateUser (x, next >> bind f) |> Free
        | Free (SendEmail (x,y,z, next)) -> SendEmail (x,y,z, next >> bind f) |> Free

    let inline private findUser uuid = FindUser (uuid, Pure) |> Free
    let inline private updateUser user = UpdateUser (user, Pure) |> Free
    let inline private sendEmail x y z = SendEmail (x,y,z, Pure) |> Free

    type private ProgramBuilder() =
        member inline __.Bind (x, f) = bind f x
        member inline __.Return x = Pure x
        member inline __.ReturnFrom x = x
        member inline __.Zero () = Pure ()

    let private program = ProgramBuilder()

    let addPoints userId pointsToAdd =
        program {
            let! u = findUser userId
            match u with
            | None -> return Some "User not found"
            | Some user ->
                let updated = { user with loyaltyPoints = user.loyaltyPoints + pointsToAdd }
                do! updateUser updated
                do! sendEmail user.email "Points added!" ("You now have " + string updated.loyaltyPoints)
                return None
        }

module Test =

    open LoyaltyPointsFree

    let rec interpret = function
        | Pure x -> x
        | Free (FindUser (x, next)) -> Some {id=x; email="a@b.c"; loyaltyPoints=0} |> next |> interpret
        | Free (UpdateUser (x, next)) -> () |> next |> interpret
        | Free (SendEmail (_,_,_, next)) -> () |> next |> interpret

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
