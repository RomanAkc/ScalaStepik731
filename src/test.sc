class Point(val x: Double, val y: Double, val z: Double) {}

object Point {
  def apply(x: Double, y: Double, z: Double): Point =
    new Point(x,y,z)

  def average(list: List[Point]): Point = list match {
    case Nil => new Point(0,0,0)
    case list if list.length == 0 => new Point(0,0,0)
    case list => new Point(
      list.map(_.x).sum / list.length,
      list.map(_.y).sum / list.length,
      list.map(_.z).sum / list.length)
  }

  def show(p: Point): String = p.x + " " + p.y + " " + p.z
}


val p1 = Point.apply(1, 2.5, 4)
val p2 = Point.apply(4, 3.5, 6)

val list = List[Point](p1, p2)

val p3 = Point.average(list)

println(Point.show(p3))