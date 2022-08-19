package truediff.consumers

import truechange.{CoreEditScript, EditScript}

trait Consumer {
  def update(editScript: EditScript): Unit =
    update(editScript.coreEdits)
  def update(editscript: CoreEditScript): Unit
}
