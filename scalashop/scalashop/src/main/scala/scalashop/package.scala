
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var yPos: Int = 0
    var xPos = clamp(x - radius, 0, src.width-1)

    var list = List[RGBA]()
    var fraction = 0
    if(radius == 0) {
      return src.apply(x,y)
    }

    while (xPos <= clamp(x + radius, 0, src.width-1)) {
      yPos = clamp(y - radius, 0, src.height-1)
      while (yPos <= clamp(y + radius, 0, src.height-1)) {
        list = src.apply(xPos, yPos) :: list
        yPos = yPos + 1
        fraction = fraction + 1
      }
      xPos = xPos + 1
    }


    rgba(list.map((r: Int) => red(r)).sum / fraction, list.map((g: Int) => green(g)).sum / fraction, list.map((b: Int) => blue(b)).sum / fraction, list.map((a: Int) => alpha(a)).sum / fraction)
  }

}
