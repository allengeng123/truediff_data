package truediff.consumers

import truechange.EditScript

trait Consumer {
  def update(editscript: EditScript): Unit
}
