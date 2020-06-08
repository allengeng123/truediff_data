package truediff.consumers

import truechange.Changeset

trait Consumer {
  def update(changeset: Changeset): Unit
}
