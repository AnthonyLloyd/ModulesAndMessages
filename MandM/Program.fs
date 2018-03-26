

type UUID = int
type User = {id:UUID; email:string; loyaltyPoints:int}

module LoyaltyPoints =

    type Saga =
        private
        | AddPoints of userId: UUID * pointsToAdd: int

    type Update =
        | StartSaga of Saga
        | UserFound of Saga * User
        | UserNotFound of Saga
        | UserUpdated of Saga * User
        | EmailSent of Saga
        
    type Model = unit

    type Command =
        | FindUser of Saga * userId: UUID
        | UpdateUser of Saga * user: User
        | SendEmail of Saga * email: string * subject: string * body: string
        | ReturnAddPoints of Saga * string option

    let update update model : Model * Command list =
        match update with
        | StartSaga (AddPoints (userId,_) as saga) ->
            model, [FindUser (saga,userId)]
        | UserFound (AddPoints(_,pointsToAdd) as saga,user) ->
            let updated = {user with loyaltyPoints = user.loyaltyPoints + pointsToAdd}
            model, [UpdateUser (saga,updated)]
        | UserNotFound saga ->
            model, [ReturnAddPoints (saga, Some "User not found")]
        | UserUpdated (saga,user) ->
            model, [SendEmail(saga, user.email, "Points added!",
                        sprintf "You now have %i" user.loyaltyPoints)]
        | EmailSent saga ->
            model, [ReturnAddPoints (saga, None)]

    // let addPoints (userId:UUID) (pointsToAdd:int) (model:Model) =
    //     update (StartSaga (AddPoints (userId,pointsToAdd)))


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
