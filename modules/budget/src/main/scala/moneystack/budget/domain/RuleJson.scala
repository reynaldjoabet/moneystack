package moneystack.budget.domain

import moneystack.platform.JsonSupport.enumCodec

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

/**
 * JSON for rule conditions and actions, used both on the wire and in the
 * `conditions` / `actions` columns. Conditions are tagged by `field`,
 * actions by `type`, as MoneyMatter's rule editor shapes them.
 */
object RuleJson {

  given Codec[TxKind]    = enumCodec(TxKind.render, TxKind.parse)
  given Codec[TextField] = enumCodec(TextField.render, TextField.parse)
  given Codec[TextOp]    = enumCodec(TextOp.render, TextOp.parse)
  given Codec[AmountOp]  = enumCodec(AmountOp.render, AmountOp.parse)
  given Codec[NoteMode]  = enumCodec(NoteMode.render, NoteMode.parse)

  private def membership(in: Boolean)  = if (in) "in" else "not_in"
  private def membershipOf(c: HCursor) = c.get[String]("operator").flatMap {
    case "in"     => Right(true)
    case "not_in" => Right(false)
    case o        => Left(DecodingFailure(s"unknown operator: $o", c.history))
  }

  given Encoder[Condition] = Encoder.instance {
    case Condition.Text(f, op, vs)      => Json.obj("field" -> f.asJson, "operator" -> op.asJson, "values" -> vs.asJson)
    case Condition.Payee(in, ids)       => Json.obj("field" -> "payee".asJson, "operator" -> membership(in).asJson, "ids" -> ids.asJson)
    case Condition.Amount(op, min, max) =>
      Json.obj("field" -> "amount".asJson, "operator" -> op.asJson, "min" -> min.asJson, "max" -> max.asJson)
    case Condition.Kind(k)            => Json.obj("field" -> "type".asJson, "operator" -> "equals".asJson, "value" -> k.asJson)
    case Condition.Wallet(in, ids)    => Json.obj("field" -> "wallet".asJson, "operator" -> membership(in).asJson, "ids" -> ids.asJson)
    case Condition.DayOfMonth(lo, hi) =>
      Json.obj("field" -> "day_of_month".asJson, "operator" -> "between".asJson, "min" -> lo.asJson, "max" -> hi.asJson)
  }

  given Decoder[Condition] = Decoder.instance { c =>
    c.get[String]("field").flatMap {
      case "title" | "note" =>
        for {
          f  <- c.get[TextField]("field")
          op <- c.get[TextOp]("operator")
          vs <- c.getOrElse[List[String]]("values")(Nil)
        } yield Condition.Text(f, op, vs)
      case "payee"  => (membershipOf(c), c.get[List[PayeeId]]("ids")).mapN(Condition.Payee.apply)
      case "amount" => (c.get[AmountOp]("operator"), c.get[Option[Long]]("min"), c.get[Option[Long]]("max")).mapN(Condition.Amount.apply)
      case "type"   => c.get[TxKind]("value").map(Condition.Kind.apply)
      case "wallet" => (membershipOf(c), c.get[List[WalletId]]("ids")).mapN(Condition.Wallet.apply)
      case "day_of_month" => (c.get[Int]("min"), c.get[Int]("max")).mapN(Condition.DayOfMonth.apply)
      case other          => Left(DecodingFailure(s"unknown condition field: $other", c.history))
    }
  }

  given Encoder[Action] = Encoder.instance {
    case Action.SetCategory(id)  => Json.obj("type" -> "set_category".asJson, "categoryId" -> id.asJson)
    case Action.AddTags(ids)     => Json.obj("type" -> "add_tags".asJson, "tagIds" -> ids.asJson)
    case Action.SetPayee(id)     => Json.obj("type" -> "set_payee".asJson, "payeeId" -> id.asJson)
    case Action.SetNote(mode, v) => Json.obj("type" -> "set_note".asJson, "mode" -> mode.asJson, "value" -> v.asJson)
  }

  given Decoder[Action] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "set_category" => c.get[CategoryId]("categoryId").map(Action.SetCategory.apply)
      case "add_tags"     => c.get[List[TagId]]("tagIds").map(Action.AddTags.apply)
      case "set_payee"    => c.get[PayeeId]("payeeId").map(Action.SetPayee.apply)
      case "set_note"     => (c.get[NoteMode]("mode"), c.get[String]("value")).mapN(Action.SetNote.apply)
      case other          => Left(DecodingFailure(s"unknown action type: $other", c.history))
    }
  }

}
