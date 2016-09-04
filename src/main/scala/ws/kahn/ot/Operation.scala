package ws.kahn.ot

import play.api.libs.json._
import play.api.libs.functional.syntax._
import ws.kahn.ot.exceptions.IncompatibleOperationsException
import scala.annotation.tailrec

/**
 * What we call an operation is essentially a list of operations on a text.
 *
 * @param components the list of operations in order. Operations can be any
 *                   one of:
 *                   - retain: advance the current position, skipping over characters
 *                   - insert: insert characters at the current position
 *                   - delete: delete characters at the current position
 * @param baseLength the length of the string that this operation operates on
 */
case class Operation(components: IndexedSeq[OperationComponent], baseLength: Int) {

  /**
   * The length of the document after applying this operation.
   */
  val targetLength = {
    var sum = 0
    components.map({
      case retain: Retain => retain.num
      case insert: Insert => insert.chars.length
      case delete: Delete => 0
    }).foreach(sum += _)
    sum
  }

  def :+(component: OperationComponent): Operation = {
    Operation(components :+ component, baseLength)
  }

  def +:(component: OperationComponent): Operation = {
    Operation(component +: components, baseLength)
  }

  override def toString: String = {
    s"""
       |Operation(
       |  ${components.map(_.toString).mkString(",\n  ")}
       |)
     """.stripMargin
  }

  /**
   * Apply the operation to a document.
   *
   * @param document
   * @return
   */
  def applyTo(document: String): String = {
    var cursor = 0
    var newDocument = document

    // Iterate over the components of this operation and apply them to the document
    for (component <- this.components) {
      component match {
        case retain: Retain => {
          cursor += retain.num
        }

        case insert: Insert => {
          newDocument = newDocument.substring(0, cursor) + insert.chars + newDocument.substring(cursor)
          cursor += insert.chars.length
        }

        case delete: Delete => {
          newDocument = newDocument.substring(0, cursor) + newDocument.substring(cursor + delete.num.toInt)
        }
      }
      //println(s"Operation 1. Cursor=$cursor. Document=$newDocument")
    }

    if (cursor != newDocument.length) {
      println("Was there an error? Cursor isn't pointing to the end of the document.")
    }

    newDocument
  }

  /**
   * Composes two operations together into one single operation.
   *
   * @param that
   * @return
   */
  def o(that: Operation): Operation = this.composeWith(that)

  /**
   * Composes two operations together into one single operation.
   *
   * @param that
   * @return
   */
  def composeWith(that: Operation): Operation = {
    if (this.targetLength != that.baseLength) {
      throw IncompatibleOperationsException(s"The target length of the left operation (${this.targetLength}) must be equal to the base length of the right operation (${that.baseLength}). These operations cannot be composed.")
    }

    // Handle some simple base cases
    if (this.components.length == 0) {
      that
    }
    else if (that.components.length == 0) {
      this
    }
    else {
      var leftIdx = 0
      var rightIdx = 0
      val leftOp = this
      val rightOp = that
      var notFinished = true
      var components = IndexedSeq.empty[OperationComponent]

      var leftComp = leftOp.components(leftIdx)
      var rightComp = rightOp.components(rightIdx)

      while (notFinished) {

        // If the first operation leads with a Delete, it can be appended directly
        // onto the new operation stack as it has no effect on the second operation.
        leftComp = leftComp match {
          case delete: Delete => {
            components = components :+ delete
            leftIdx += 1
            leftOp.components(leftIdx)
          }
          case _ => leftComp
        }

        // If the second operation leads with an Insert, it can be appended directly
        // onto the new operation stack as it has no effect on the first operation.
        rightComp = rightComp match {
          case insert: Insert => {
            components = components :+ insert
            rightIdx += 1
            rightOp.components(rightIdx)
          }
          case _ => rightComp
        }

        // Now handle the remaining possible combinations at the head of each
        // operation. We are essentially operating on the joining line between
        // the two operations: the target length of the left operation, and the
        // base length of the right operation.
        //
        // For each iteration we will have the case where the left
        // operation is longer, the right operation is longer, or both operations
        // are equal in length.
        (leftComp, rightComp) match {
          // Retain + Retain
          case (retainL: Retain, retainR: Retain) => {
            // L > R
            if (retainL.num > retainR.num) {
              components = components :+ retainR

              leftComp = Retain(retainL.num - retainR.num)
              rightIdx += 1
              rightComp = rightOp.components(rightIdx)
            }
            // L == R
            else if (retainL.num == retainR.num) {
              components = components :+ retainR

              leftIdx += 1
              rightIdx += 1

              if (leftIdx >= leftOp.components.length &&
                rightIdx >= rightOp.components.length) {
                notFinished = false
              }
              else {
                leftComp = leftOp.components(leftIdx)
                rightComp = rightOp.components(rightIdx)
              }
            }
            // L < R
            else {
              components = components :+ retainL

              rightComp = Retain(retainR.num - retainL.num)
              leftIdx += 1
              leftComp = leftOp.components(leftIdx)
            }
          }

          // Retain + Delete
          case (retainL: Retain, deleteR: Delete) => {
            if (retainL.num > deleteR.num) {
              components = components :+ deleteR

              rightIdx += 1
              leftComp = Retain(retainL.num - deleteR.num)
              rightComp = rightOp.components(rightIdx)
            }
            else if (retainL.num == deleteR.num) {
              components = components :+ deleteR

              leftIdx += 1
              rightIdx += 1

              if (leftIdx >= leftOp.components.length &&
                rightIdx >= rightOp.components.length) {
                notFinished = false
              }
              else {
                leftComp = leftOp.components(leftIdx)
                rightComp = rightOp.components(rightIdx)
              }
            }
            else {
              components = components :+ Delete(retainL.num)

              leftIdx += 1
              leftComp = leftOp.components(leftIdx)
              rightComp = Delete(deleteR.num - retainL.num)
            }
          }

          // Insert + Retain
          case (insertL: Insert, retainR: Retain) => {
            if (insertL.chars.length > retainR.num) {
              components = components :+ Insert(insertL.chars.take(retainR.num))

              rightIdx += 1
              leftComp = Insert(insertL.chars.substring(retainR.num))
              rightComp = rightOp.components(rightIdx)
            }
            else if (insertL.chars.length == retainR.num) {
              components = components :+ insertL

              leftIdx += 1
              rightIdx += 1

              if (leftIdx >= leftOp.components.length &&
                  rightIdx >= rightOp.components.length) {
                notFinished = false
              }
              else {
                leftComp = leftOp.components(leftIdx)
                rightComp = rightOp.components(rightIdx)
              }
            }
            else {
              components = components :+ insertL

              leftIdx += 1
              leftComp = leftOp.components(leftIdx)
              rightComp = Retain(retainR.num - insertL.chars.length)
            }
          }

          // Insert + Delete
          case (insertL: Insert, deleteR: Delete) => {
            if (insertL.chars.length > deleteR.num) {
              rightIdx += 1
              leftComp = Insert(insertL.chars.substring(deleteR.num))
              rightComp = rightOp.components(rightIdx)
            }
            else if (insertL.chars.length == deleteR.num) {
              // Insert and delete exactly cancel each other out. Do nothing and advance the pointers!

              leftIdx += 1
              rightIdx += 1

              if (leftIdx >= leftOp.components.length &&
                rightIdx >= rightOp.components.length) {
                notFinished = false
              }
              else {
                leftComp = leftOp.components(leftIdx)
                rightComp = rightOp.components(rightIdx)
              }
            }
            else {
              leftIdx += 1
              leftComp = leftOp.components(leftIdx)
              rightComp = Delete(deleteR.num - insertL.chars.length)
            }
          }

          // The other combinations (Delete Insert, Delete Retain, Retain Insert)
          // should not be possible as we take care of left deletes and right inserts
          // before the loop.
          case _ => throw new Exception("You should never reach this point, so something went horribly wrong.")
        }

      }

      // Build the composed operation
      val composedOp = Operation(components, this.baseLength)

      // Verify that the target length matches that of the "right" operation.
      if (composedOp.targetLength != that.targetLength) {
        throw CompositionErrorException(s"Composition error. The target length of the composed operation (${composedOp.targetLength}) did not equal the target length of the right operation (${that.targetLength})!")
      }

      // Finally return the composed operation
      //composedOp
      optimize(composedOp)
    }
  }

  /**
   * Optimize an operation by combining adjacent components of the same type.
   *
   * This is a pretty simple/naive function... for now it doesn't re-order the
   * components to make them more semantically correct.
   *
   * @param operation
   * @return
   */
  private def optimize(operation: Operation): Operation = {
    if (operation.components.length > 1) {
      var i = 0
      var j = 1
      var merged = IndexedSeq.empty[OperationComponent]

      var headA = operation.components(i)
      var headB = operation.components(j)
      var shouldContinue = true

      while (shouldContinue) {
        (headA, headB) match {
          case (retainA: Retain, retainB: Retain) => {
            val newRetain = Retain(retainA.num + retainB.num)

            headA = newRetain

            j += 1

            if (j >= operation.components.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = operation.components(j)
            }
          }
          case (insertA: Insert, insertB: Insert) => {
            val newInsert = Insert(insertA.chars + insertB.chars)
            headA = newInsert
            j += 1
            if (j >= operation.components.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = operation.components(j)
            }
          }
          case (deleteA: Delete, deleteB: Delete) => {
            val newDelete = Delete(deleteA.num + deleteB.num)
            headA = newDelete
            j += 1
            if (j >= operation.components.length) {
              shouldContinue = false
              merged = merged :+ headA
            }
            else {
              headB = operation.components(j)
            }
          }
//          case (deleteA: Delete, insertB: Insert) => {
//            headA = insertB
//            headB = deleteA
//          }
          case (anyA, anyB) => {
            merged = merged :+ anyA
            i = j
            j += 1

            if (i >= operation.components.length ||
                j >= operation.components.length) {
              shouldContinue = false
              merged = merged :+ anyB
            }
            else {
              headA = operation.components(i)
              headB = operation.components(j)
            }
          }
        }
      }

      val optimized = Operation(merged, operation.baseLength)
      optimized
    }
    else {
      operation
    }
  }

  /**
   * An alias for the transform method.
   *
   * @param that
   * @return
   */
  def x(that: Operation): (Operation, Operation) = Operation.transform(this, that)
}

object Operation {

  implicit val reads: Reads[Operation] = (
    (__ \ "components").read[IndexedSeq[OperationComponent]] and
      (__ \ "baseLength").read[Int]
    )(Operation.apply _)

  implicit val writes: Writes[Operation] = (
    (__ \ "components").write[IndexedSeq[OperationComponent]] and
      (__ \ "baseLength").write[Int]
    )(unlift(Operation.unapply))

  /**
   * Transform two operations that should be applied "simultaneously" to the same document.
   *
   * In times of conflict, the Alpha operation always takes precedence over the Beta operation.
   *
   * @param leftOp
   * @param rightOp
   * @return
   */
  def transform(leftOp: Operation, rightOp: Operation): (Operation, Operation) = {
    var aIdx = 0
    var bIdx = 0
    var leftOps = leftOp.components
    var rightOps = rightOp.components

    var shouldContinue = true

    var leftComp = leftOps(aIdx)
    var rightComp = rightOps(bIdx)

    var transformedA = IndexedSeq.empty[OperationComponent]
    var transformedB = IndexedSeq.empty[OperationComponent]

    while (shouldContinue) {
      (leftComp, rightComp) match {
        // If leftComp contains an Insert, insert it, retain its characters in the other
        // operation, and advance the leftComp pointer.
        case (insertA: Insert, _) => {
          transformedA = transformedA :+ insertA
          transformedB = transformedB :+ Retain(insertA.chars.length)

          aIdx += 1
          if (aIdx == leftOps.length) {
            leftComp = Retain(0)
          }
          else {
            leftComp = leftOps(aIdx)
          }
        }

        // Same thing for rightComp containing an insert, except Alpha's inserts happen first.
        case (_, insertB: Insert) => {
          transformedA = transformedA :+ Retain(insertB.chars.length)
          transformedB = transformedB :+ insertB

          bIdx += 1
          if (bIdx == rightOps.length) {
            rightComp = Retain(0)
          }
          else {
            rightComp = rightOps(bIdx)
          }
        }

        // Both retain. Easy... both retain the same amount, slice off the longer one,
        // and iterate again.
        case (retainA: Retain, retainB: Retain) => {
          // A > B
          if (retainA.num > retainB.num) {
            transformedA = transformedA :+ retainB
            transformedB = transformedB :+ retainB

            leftComp = Retain(retainA.num - retainB.num)

            bIdx += 1
            rightComp = rightOps(bIdx)
          }
          // A == B
          else if (retainA.num == retainB.num) {
            transformedA = transformedA :+ retainA
            transformedB = transformedB :+ retainA

            aIdx += 1
            bIdx += 1

            if (aIdx >= leftOp.components.length &&
                bIdx >= rightOp.components.length
            ) {
              shouldContinue = false
            }
            else {
              leftComp = leftOps(aIdx)
              rightComp = rightOps(bIdx)
            }
          }
          // A < B
          else {
            transformedA = transformedA :+ retainA
            transformedB = transformedB :+ retainA

            rightComp = Retain(retainB.num - retainA.num)

            aIdx += 1
            leftComp = leftOps(aIdx)
          }
        }

        case (deleteA: Delete, retainB: Retain) => {
          // A > B
          if (deleteA.num > retainB.num) {
            transformedA = transformedA :+ Delete(retainB.num)
            leftComp = Delete(deleteA.num - retainB.num)

            bIdx += 1
            rightComp = rightOps(bIdx)
          }
          // A == B
          else if (deleteA.num == retainB.num) {
            transformedA = transformedA :+ deleteA

            aIdx += 1
            bIdx += 1

            if (aIdx >= leftOp.components.length &&
              bIdx >= rightOp.components.length
            ) {
              shouldContinue = false
            }
            else {
              leftComp = leftOps(aIdx)
              rightComp = rightOps(bIdx)
            }
          }
          // A < B
          else {
            transformedA = transformedA :+ deleteA
            rightComp = Retain(retainB.num - deleteA.num)

            aIdx += 1
            leftComp = leftOps(aIdx)
          }
        }

        case (retainA: Retain, deleteB: Delete) => {
          // A > B
          if (retainA.num > deleteB.num) {
            transformedB = transformedB :+ deleteB
            leftComp = Retain(retainA.num - deleteB.num)
            bIdx += 1
            rightComp = rightOps(bIdx)
          }
          // A == B
          else if (retainA.num == deleteB.num) {
            transformedB = transformedB :+ deleteB

            aIdx += 1
            bIdx += 1

            if (aIdx >= leftOp.components.length &&
              bIdx >= rightOp.components.length
            ) {
              shouldContinue = false
            }
            else {
              leftComp = leftOps(aIdx)
              rightComp = rightOps(bIdx)
            }
          }
          // A < B
          else {
            transformedB = transformedB :+ Delete(retainA.num)
            rightComp = Delete(deleteB.num - retainA.num)
            aIdx += 1
            leftComp = leftOps(aIdx)
          }
        }

        case (deleteA: Delete, deleteB: Delete) => {
          // A > B
          if (deleteA.num > deleteB.num) {
            leftComp = Delete(deleteA.num - deleteB.num)

            bIdx += 1
            rightComp = rightOps(bIdx)
          }
          // A == B
          else if (deleteA.num == deleteB.num) {
            aIdx += 1
            bIdx += 1

            if (aIdx >= leftOp.components.length &&
              bIdx >= rightOp.components.length
            ) {
              shouldContinue = false
            }
            else {
              leftComp = leftOps(aIdx)
              rightComp = rightOps(bIdx)
            }
          }
          // A < B
          else {
            rightComp = Delete(deleteB.num - deleteA.num)

            aIdx += 1
            leftComp = leftOps(aIdx)
          }
        }
      }
    }

    (Operation(transformedA, rightOp.targetLength), Operation(transformedB, leftOp.targetLength))
  }


}