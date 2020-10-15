package truediff.compat.gumtree

import com.github.gumtreediff.tree.{ITree, TreeContext}

class DiffableGumTreeContext extends TreeContext {
  override def createTree(typ: Int, label: String, typeLabel: String): ITree = {
    registerTypeLabel(typ, typeLabel)
    new DiffableGumTree(typeLabel, label)
  }
}
