package truechange.changeable

import truechange.{Change, Changeset}

trait ChangeableTree {
  def applyChangeset(changeset: Changeset): Unit =
    changeset.foreach(this.change)

  def change(change: Change): Unit
}
