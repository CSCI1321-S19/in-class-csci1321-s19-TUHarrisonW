package Dr.Mario

case class PassableBoard(cells: Seq[PassableCell], drawCurrent: Boolean,
    pp1: PassableCell, pp2: PassableCell)