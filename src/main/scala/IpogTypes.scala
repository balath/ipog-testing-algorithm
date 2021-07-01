object IpogTypes {

  case class Parameter(name: String, dimension: Dimension)

  type Dimension = Int
  type ParamComb = Vector[Int]
  type ValuesComb = Vector[Option[Int]]
  type PiList = Map[ParamComb, Vector[ValuesComb]]

}
