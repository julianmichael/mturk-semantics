package mts.util

object jvm {
  def sendToClipboard(s: String): Unit = {
    import java.awt._;
    import java.awt.datatransfer._;
    import java.io._;
    val selection = new StringSelection(s)
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, selection)
  }
}
