class Rational(_numer: Int, _denom: Int) {
  require(_denom != 0, "Denominator must be positive")

  private def _gcd(a: Int, b: Int): Int = if (b == 0) a else _gcd(b, a % b)
  private val _g = _gcd(_numer, _denom)

  def numer = _numer / _g
  def denom = _denom / _g

  def add(other: Rational) = new Rational(
    numer * other.denom + denom * other.numer,
    denom * other.denom
  )

  def sub(other: Rational) = new Rational(
    numer * other.denom - denom * other.numer,
    denom * other.denom
  )

  def less(other: Rational) = numer * other.denom < denom * other.numer

  def neg = new Rational(-numer, denom)

  override def toString = numer + "/" + denom
}


val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.sub(y).sub(z)
y.add(y)