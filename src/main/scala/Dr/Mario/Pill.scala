package Dr.Mario

class Pill(val cells: List[PillPiece]) extends BoardElement {
  def fall(): Pill = {
    new Pill(cells.map(pp => pp.fall()))
  }
}