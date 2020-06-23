package truediff.consumers

import truechange.Editscript

trait Consumer {
  def update(changeset: Editscript): Unit
}
