package engine.grid

import scala.reflect.ClassTag
import engine.grid.grid.{gridToTriGrid, triGridToGrid}
import scala.util.{Failure, Success, Try}

/** The trait `TriGrid` represents hexagonal grids that contain elements of a particular
  * kind. Each element in a grid is located at a unique pair of coordinates, represented
  * as a [[GridPos]].
  *
  * Since `TriGrid` represents hexagonal grid that is made of triangles, it will be fairly
  * different from normal 2D-Cartesian grid system. A `TriGrid` can be seen as a normal grid
  * system with missing coordinates. Y coordinates are equivalent to the rows of the hexagonal
  * grid, and X coordinates indicate the indice of the triangles in that row.
  *
  * X coordinates run from 0 to `width-1`, Y coordinates from 0 to `height-1`.
  * (0,0) corresponds to the upper left corner of the grid.
  *
  * X coordinates depend on Y coordinates and * X may not equal to `width-1` for some Y value.
  *
  * @example For example, in a hexagon made of 24 triangles (4 rows, first and fourth row have 5 triangles
  * while second and third row have 7 triangles), for the first row (`y == 0`),
  * `x` only ranges from 0 to 4, and for the second row (`y == 1`), `x = 0..6`.
  *
  * @see [[GridPos]], [[TriGridPos]]
  *
  * There are different kinds of grids: the type of element that a grid contains is
  * defined by the grid’s type parameter. For instance, `TriGrid[TriHolder]` is a grid where
  * each pair of x and y coordinates contains a `TriHolder` object, and `TriGrid[House]` is
  * a grid containing `House` objects. The `TriGrid` trait was originally for reproducity
  * and scalability when it comes to bigger and more complex projects. In the scope of this
  * project, `TriGrid` objects will only contains `TriHolder`. As the scope of the project
  * is relatively small (only for a game), the reproducibility of this trait is not utilized.
  * This can explain for some specific methods of the trait.
  *
  * A `TriGrid` is immutable.
  *
  * Upon creation, a `TriGrid` initializes itself by calling [[initialElements]], which
  * produces an initial state for the grid.
  *
  * @param width   the number of elements in each row of the grid
  * @param height  the number of elements in each column of the grid */
trait TriGrid[Element: ClassTag](val width: Int, val height: Int):

  /** The number of elements in this hexagon grid, in total. */
  val size: Int =
    var count = 0
    for i <- 0 until height/2 do
      count += width - 2*i
    count * 2


  /**
    * Return the sizes of the sub-arrays correspond to width and height.
    * 
    * For example, board size 24 will have sub-array of size 5, 7, 7, 5 respectively.
    * 
    * The general formula: size = x-2i for floor(y/2)-1 to 0 and x-2i for 0 to floor(y/2)-1
    */
  private val subArrSize: Seq[Int] =
    val halfHeight = this.height / 2
    val upperHalf = (halfHeight-1 to 0 by -1).map(i => width - 2*i)
    val lowerHalf = (0 until halfHeight).map(i => width - 2*i)
    (upperHalf ++ lowerHalf).toSeq


  /**
    * Split the array of all elements into subarrays of hexagon row size.
    * 
    * For example, hexagon board with 24 tiles will have 4 rows,
    * and the length of the rows are 5, 7, 7, 5 respectively.
    */
  private val contents: Array[Array[Element]] =
    val elems = try this.initialElements catch case npe: NullPointerException => throw RuntimeException("Grid initialization failed with a NullPointerException.\nPossible cause: trying to access an element’s (still nonexistent) neighbors or other parts of the unready grid while initializing.", npe)
    println(this.size)
    println(this.subArrSize)
    println(this.subArrSize.sum)
    require(elems.sizeIs == this.size, s"The number of elements returned by initialElements (${elems.size}) did not equal board size(${this.size}).")
    require(this.subArrSize.sum == this.size, s"The sub-array length does not sum up to ${this.size}.")
    val listOfElems = elems.toList
    val subArrays = this.subArrSize.foldLeft((listOfElems, List.empty[Array[Element]])){
      case ((remaining, subArrays), size) =>
        val (subArray, rest) = remaining.splitAt(size)
        (rest, subArrays :+ subArray.toArray)
    }._2.toArray
    subArrays


  /** Returns the element at the given pair of coordinates.
    * @param location  a location on the grid (which must be within range or this method will fail with an error) */
  def elementAt(location: GridPos) =
    require(this.contains(location), s"Attempted to access a non-existent location $location of a ${width}-by-${height} grid.")
    this.contents(location.x)(location.y)


  /** Checks whether the grid contains the given x and y coordinates. */
  private def contains(x: Int, y: Int): Boolean =
    gridToTriGrid.contains((x,y))


  /** Determines whether the grid contains the given pair of coordinates.
    * For instance, a grid with a width and height of 5 will contain (0, 0)
    * and (4, 4) but not (-1, -1), (4, 5) or (5, 4). */
  def contains(location: GridPos): Boolean = this.contains(location.x, location.y)


  /** Generates the elements that initially occupy the grid. This method is automatically
    * invoked by every `Grid` object upon creation. Subtypes should implement this method
    * as appropriate for the particular sort of grid desired.
    *
    * Note that since this method produces the `Grid`’s initial contents, it gets called
    * during initialization before the `Grid` actually has any elements as content. Therefore,
    * subtypes’ implementations of this method must not depend on the state of the new `Grid`
    * (by calling `neighbors` or `elementAt`, for instance) or attempt to modify the `Grid`
    * (with `update`, for instance).
    *
    * This method is meant for initialization purposes only. To access all the elements of
    * an already-initialized `Grid`, call [[allElements]] instead.
    *
    * @return a collection of size `width` times `height` that contains the initial grid elements.
    *         The first element in the collection will appear at `GridPos` (0,0), the second at (1,0),
    *         and so on, filling in the first row before continuing on the second row at (0,1). */
  def initialElements: Seq[Element]


  /** Returns the element at the given pair of coordinates. (This does the same as `elementAt`.)
    * @param location  a location on the grid (which must be within range or this method
    *                  will fail with an error) */
  def apply(location: GridPos) = this.elementAt(location)


  /** Returns a collection of all the locations on the grid. */
  def allPositions: Seq[GridPos] =
    gridToTriGrid.keys.toSeq.sorted.map((x,y) => GridPos(x,y))


  /** Returns a collection of all the elements currently in the grid. */
  def allElements: Seq[Element] =
    for pos <- this.allPositions; elem <- Option(this(pos)) yield elem

end TriGrid


