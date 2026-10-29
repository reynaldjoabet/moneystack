package moneystack.platform

/** Minimal RFC 4180 helpers: quoted cells, doubled quotes inside them. */
object Csv {

  def split(line: String): List[String] = {
    val out    = List.newBuilder[String]
    val cell   = new StringBuilder
    var quoted = false
    var i      = 0
    while (i < line.length) {
      val c = line.charAt(i)
      if (quoted) {
        if (c == '"' && i + 1 < line.length && line.charAt(i + 1) == '"') { cell += '"'; i += 1 }
        else if (c == '"') quoted = false
        else cell += c
      } else if (c == '"') quoted = true
      else if (c == ',') { out += cell.result(); cell.clear() }
      else cell += c
      i += 1
    }
    out += cell.result()
    out.result()
  }

  /** Quotes a cell when it holds a comma, quote or line break. */
  def cell(s: String): String =
    if (s.exists(c => c == ',' || c == '"' || c == '\n' || c == '\r')) "\"" + s.replace("\"", "\"\"") + "\"" else s

  def row(cells: List[String]): String = cells.map(cell).mkString(",")
}
