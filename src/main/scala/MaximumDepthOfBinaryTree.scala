object MaximumDepthOfBinaryTree {
// https://leetcode.com/problems/maximum-depth-of-binary-tree/

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object TreeNode {
    def apply(value: Int, left: TreeNode, right: TreeNode): TreeNode = new TreeNode(value, left, right)

    def unapply(arg: TreeNode): Option[(Int, TreeNode, TreeNode)] = Some(arg.value, arg.left, arg.right)
  }

  object Solution {

    def maxDepth(root: TreeNode): Int = {
      if (root == null) 0
      else root match {
        case TreeNode(_, null, null) => 1
        case TreeNode(_, null, right) => maxDepth(right) + 1
        case TreeNode(_, left, null) => maxDepth(left) + 1
        case TreeNode(_, left, right) => maxDepth(left) + 1 max maxDepth(right) + 1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val tree = TreeNode(3, TreeNode(9, null, null), TreeNode(20, TreeNode(15, null, null), TreeNode(7, null, null)))
    println(Solution.maxDepth(tree))
    println(Solution.maxDepth(TreeNode(0, null, null)))
    println(Solution.maxDepth(null))
  }
}
