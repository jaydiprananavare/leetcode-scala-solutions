import scala.annotation.tailrec

object AddTwoNumbers {
  //  https://leetcode.com/problems/add-two-numbers/

  //    Definition for singly-linked list.
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x

    override def toString: String = s"ListNode($x, ${if (next == null) "null" else next.toString})"
  }

  //       9,9,9,9,9,9,9
  //       9,9,9,9
  //sum   18,19,19,19,10,10,10,1
  //digit  8,9,9,9,0,0,0,1

  object Solution {
    object ListNode {
      def apply(_x: Int, _next: ListNode): ListNode = new ListNode(_x, _next)

      def unapply(arg: ListNode): Option[(Int, ListNode)] = Some(arg.x, arg.next)
    }

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

      @tailrec
      def joinList(l1: ListNode, l2: ListNode, lastNode: ListNode, carry: Int = 0): ListNode = {
        (l1, l2) match {
          case (null, null) if carry == 0 =>
            lastNode

          case (null, null) =>
            val node = new ListNode(carry, null)
            lastNode.next = node
            node

          case (ListNode(x, null), null) =>
            val sum = x + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(null, null, node, sum / 10)

          case (ListNode(x, l1Next), null) =>
            val sum = x + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(l1Next, null, node, sum / 10)

          case (null, ListNode(y, null)) =>
            val sum = y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(null, null, node, sum / 10)

          case (null, ListNode(y, l2Next)) =>
            val sum = y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(null, l2Next, node, sum / 10)

          case (ListNode(x, null), ListNode(y, null)) =>
            val sum = x + y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(null, null, node, sum / 10)

          case (ListNode(x, l1Next), ListNode(y, null)) =>
            val sum = x + y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(l1Next, null, node, sum / 10)

          case (ListNode(x, null), ListNode(y, l2Next)) =>
            val sum = x + y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(null, l2Next, node, sum / 10)

          case (ListNode(x, l1Next), ListNode(y, l2Next)) =>
            val sum = x + y + carry
            val node = new ListNode(sum % 10, null)
            lastNode.next = node
            joinList(l1Next, l2Next, node, sum / 10)
        }
      }


      val root = new ListNode()
      joinList(l1, l2, root)
      root.next

    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.addTwoNumbers(new ListNode(2, new ListNode(4, new ListNode(3))), new ListNode(5, new ListNode(6, new ListNode(4)))))
    println(Solution.addTwoNumbers(new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9))))))), new ListNode(9, new ListNode(9, new ListNode(9, new ListNode(9))))))
    println(Solution.addTwoNumbers(new ListNode(0), new ListNode(0)))
  }
}
