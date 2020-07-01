package truediff.consumers

import truechange.EditScript

trait Consumer {
  def update(changeset: EditScript): Unit
}
