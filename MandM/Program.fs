﻿





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
