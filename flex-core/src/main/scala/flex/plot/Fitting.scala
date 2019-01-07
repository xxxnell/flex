package flex.plot

import org.apache.commons.math3.fitting.{PolynomialCurveFitter, WeightedObservedPoints}

import scala.math.BigDecimal
import scala.util.Try

object Fitting {

  def apply(as: List[(Double, Double)], x: Double): Option[Double] = dataFitting(as, x)

  /**
   * @param as List of (x, y)
   * */
  def dataFitting(as: List[(Double, Double)], x: Double): Option[Double] = {
    lazy val largeCutoff = 1e300
    lazy val smallCutoff = -1e300
    lazy val polyValid = as.forall {
      case (_x, _y) =>
        _x < largeCutoff && _y < largeCutoff && _x > smallCutoff && _y > smallCutoff
    }

    as match {
      case start :: end :: Nil if start._1 >= x && end._1 <= x => linearFitting(start, end, x)
      case _ if as.size > 2 && polyValid => polynomialFitting(as, x)
      case _ =>
        for {
          start <- as.find(_._1 <= x)
          end <- as.find(_._1 >= x)
          fitting <- linearFitting(start, end, x)
        } yield fitting
    }
  }

  def linearFitting(a1: (Double, Double), a2: (Double, Double), x: Double): Option[Double] = {
    val (x1, y1) = a1
    val (x2, y2) = a2
    lazy val p = linearFittingDouble(a1, a2, x)

    if (!p.isNaN) Some(p)
    else
      try {
        Some(
          linearFittingBigDecimal(
            (BigDecimal(x1), BigDecimal(y1)),
            (BigDecimal(x2), BigDecimal(y2)),
            BigDecimal(x)
          ).toDouble
        )
      } catch {
        case _: Exception => None
      }
  }

  def linearFittingDouble(a1: (Double, Double), a2: (Double, Double), x: Double): Double = {
    val (x1, y1) = a1
    val (x2, y2) = a2

    if (y1 == y2) y1
    else if (x1 == x2) y1 + (y2 - y1) / 2
    else if (!x1.isInfinity && !x2.isInfinity) {
      lazy val slope1 = (y2 - y1) / (x2 - x1)
      lazy val slope2 = y2 * ((1 - y1 / y2) / (1 - x1 / x2)) / x2
      lazy val slope3 = y1 * ((1 - y2 / y1) / (1 - x2 / x1)) / x1
      lazy val slope4 = y1 / ((1 - x2 / x1) * x1)
      lazy val slope5 = y1 / ((x1 / x2 - 1) * x2)

      val slope =
        if (!slope1.isNaN && !slope1.isInfinity) slope1
        else if (!slope2.isNaN && !slope2.isInfinity) slope2
        else if (!slope3.isNaN && !slope3.isInfinity) slope3
        else if (!slope4.isNaN && !slope4.isInfinity && math.abs(y2 / y1) < 1 && x1 != 0) slope4
        else if (!slope5.isNaN && !slope5.isInfinity && math.abs(y2 / y1) < 1 && x2 != 0) slope5
        else Double.NaN
      val c = y1 - slope * x1

      val interp = slope * x + c

      if (!interp.isNaN && !interp.isInfinity) interp else Double.NaN
      // todo throw an exception when x is not sim to x1B
    } else Double.NaN
  }

  def linearFittingBigDecimal(a1: (BigDecimal, BigDecimal), a2: (BigDecimal, BigDecimal), x: BigDecimal): BigDecimal = {
    val (x1, y1) = a1
    val (x2, y2) = a2

    if (y1 == y2) y1
    else if (x1 == x2) y1 + (y2 - y1) / 2
    else {
      val slope = (y2 - y1) / (x2 - x1)
      val c = y1 - slope * x1

      slope * x + c
    }
  }

  def polynomialFitting(as: List[(Double, Double)], x: Double): Option[Double] =
    Try {
      val xB = BigDecimal(x)

      val obs = as.foldLeft(new WeightedObservedPoints) { case (_obs, (_x, _y)) => _obs.add(_x, _y); _obs }
      val fitter = PolynomialCurveFitter.create(2)
      val coeff = fitter.fit(obs.toList)

      (coeff(0) * xB * xB + coeff(1) * xB + coeff(2)).toDouble
    }.toOption

}
