package turksem.gapfill

import cats.Show

import monocle.macros.Lenses

@Lenses case class TriggerSlot(
  label: String,
  isTrigger: Boolean = false) {
  def withLabel(newLabel: String) = TriggerSlot(newLabel, isTrigger)
  def withIsTrigger(newIsTrigger: Boolean) = TriggerSlot(label, newIsTrigger)
}
object TriggerSlot {
  implicit val triggerSlotShow = new Show[TriggerSlot] {
    override def show(ts: TriggerSlot) = if(ts.isTrigger) ts.label + "*" else ts.label
  }
}
