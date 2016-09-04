package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._

sealed trait OperationComponent {
  val opType: Char
}

object OperationComponent {
  implicit val opReads = new Reads[OperationComponent] {
    def reads(json: JsValue) = {
      json match {
        case number: JsNumber => {
          val num: Int = number.as[Int]
          if (num >= 0) {
            JsSuccess(Retain(num))
          }
          else {
            JsSuccess(Delete(num * -1))
          }
        }
        case string: JsString => JsSuccess(Insert(string.as[String]))
        case _ => JsError("Not a valid operation. It must either be a string or a number.")
      }
    }
  }

  implicit val opWrites = new Writes[OperationComponent] {
    def writes(opc: OperationComponent): JsValue = {
      opc match {
        case retain: Retain => JsNumber(retain.num)
        case insert: Insert => JsString(insert.chars)
        case delete: Delete => JsNumber(delete.num * -1)
      }
    }
  }
}

/**
 * The "retain" operation moves the cursor along the document by
 * a number of characters, effectively "skipping" over them.
 *
 * NB: comparison methods do not work exactly as one might expect.
 *     The equals (==) operator only compares the number of affected
 *     characters! Retain(5) == Insert("kitty") is true!
 *
 *     To test whether two operations are truly "equal" (and have the same type)
 *     use the triple-equals operator (===). Retain(5) === Insert("kitty") is false
 *     but Insert("kitty") === Insert("kitty") is true!
 *
 *     The remaining comparison operators (>, >=, <, <=) also compare against
 *     the num or chars.length values.
 *
 * @param num the number of characters to skip or retain.
 */
case class Retain(num: Int) extends OperationComponent {
  override val opType: Char = 'r'
  override def toString: String = s"Retain($num)"
}

/**
 * The "insert" operation inserts a given string at the cursor's current
 * position in the string.
 *
 * @param chars the string to insert
 */
case class Insert(chars: String) extends OperationComponent {
  override val opType: Char = 'i'
  override def toString: String = s"""Insert(\"${chars}\")"""

}

/**
 * The "delete" operation deletes a number of characters starting
 * from the cursor's current position in the document.
 *
 * @param num the number of characters to delete.
 */
case class Delete(num: Int) extends OperationComponent {
  override val opType: Char = 'd'
  override def toString: String = s"Delete($num)"
}