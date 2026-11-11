package moneystack.it

import moneystack.Main
import moneystack.db.{Portfolios, Recommendations, Users}
import moneystack.domain.*
import moneystack.external.{Push, Telegram}
import moneystack.service.RecommendationService

import cats.effect.*
import cats.effect.std.Supervisor
import org.typelevel.log4cats.noop.NoOpLogger
import skunk.Session

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/** Publishing a recommendation notifies the model's followers, and only them, in the background. */
class RecommendationIntegrationSuite extends moneystack.testkit.PostgresSuite[RecommendationIntegrationSuite.Env] {
  import RecommendationIntegrationSuite.*

  def migrations: List[String] = Main.migrations

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Env] =
    for {
      sent          <- Resource.eval(Ref[IO].of(List.empty[String]))
      failFor       <- Resource.eval(Ref[IO].of(Set.empty[UserId]))
      notifications <- Supervisor[IO](await = true)
      telegram = new Telegram[IO] {
        def sendMessage(chatId: TelegramChatId, text: String): IO[Unit]                 = sent.update(s"telegram ${chatId.value}" :: _)
        def broadcastToInvestors(chatIds: List[TelegramChatId], text: String): IO[Unit] = IO.unit
      }
      push = new Push[IO] {
        def send(userId: UserId, title: Title, body: Body): IO[Unit] =
          failFor.get.flatMap(f =>
            if (f(userId)) IO.raiseError(new RuntimeException("push provider is down"))
            else sent.update(s"push ${userId.value}" :: _)
          )
      }
      portfolios = Portfolios.make[IO](pool)
      service    = RecommendationService.make[IO](Recommendations.make[IO](pool), portfolios, telegram, push, notifications, NoOpLogger[IO])
    } yield Env(service, Users.make[IO](pool), portfolios, sent, failFor)

  private def user(chat: Option[Long] = None): IO[User] =
    env.users.create(User(
      UserId.assume(UUID.randomUUID()),
      Email.applyUnsafe(s"u-${UUID.randomUUID()}@example.com"),
      PasswordHash.applyUnsafe("x"),
      DisplayName.applyUnsafe("Test"),
      Role.Investor,
      Locale.EN,
      chat.map(TelegramChatId.assume),
      Instant.EPOCH
    ))

  private def model(owner: User): IO[ModelPortfolio] =
    env.portfolios.createModel(
      ModelPortfolio(
        ModelPortfolioId.assume(UUID.randomUUID()),
        Title.applyUnsafe("Growth"),
        PortfolioStrategy.Growth,
        owner.id,
        Instant.EPOCH,
        Instant.EPOCH,
        None
      ),
      Nil
    )

  private def follow(u: User, m: Option[ModelPortfolio]): IO[Portfolio] =
    env.portfolios.createPortfolio(
      Portfolio(PortfolioId.assume(UUID.randomUUID()), u.id, Title.applyUnsafe("Mine"), CurrencyCode.USD, m.map(_.id), Instant.EPOCH)
    )

  /** Waits for `n` sends in the background, then a moment more so that an extra one would show up too. */
  private def sent(n: Int): IO[List[String]] =
    env.sent.get.iterateUntil(_.size >= n).timeout(5.seconds) >> IO.sleep(200.millis) >> env.sent.get

  test("followers get a push, and Telegram if linked; a failed send doesn't stop the rest") {
    for {
      analyst <- user()
      growth  <- model(analyst)
      other   <- model(analyst)
      withTg  <- user(chat = Some(42L))
      plain   <- user()
      broken  <- user()
      twice   <- user()
      outside <- user(chat = Some(7L))
      _       <- follow(withTg, Some(growth)) >> follow(plain, Some(growth)) >> follow(broken, Some(growth))
      _       <- follow(twice, Some(growth)) >> follow(twice, Some(growth)) >> follow(outside, Some(other)) >> follow(outside, None)
      _       <- env.failFor.set(Set(broken.id))
      _       <- env.sent.set(Nil)
      _       <- env.service.publish(
        analyst.id,
        growth.id,
        Ticker.applyUnsafe("AAPL"),
        Exchange.applyUnsafe("NASDAQ"),
        TradeAction.Buy,
        Body.applyUnsafe("Strong quarter"),
        None
      )
      got <- sent(4)
    } yield assertEquals(
      got.sorted,
      List(s"push ${plain.id.value}", s"push ${twice.id.value}", s"push ${withTg.id.value}", "telegram 42").sorted,
      "every follower once, Telegram only where linked, no one outside the model, and the failure skipped"
    )
  }
}

object RecommendationIntegrationSuite {
  final case class Env(
      service: RecommendationService[IO],
      users: Users[IO],
      portfolios: Portfolios[IO],
      sent: Ref[IO, List[String]],
      failFor: Ref[IO, Set[UserId]]
  )
}
