package truediff.consumers

import truediff.changeset.Changeset

trait Consumer {
  def update(changeset: Changeset): Unit
}
