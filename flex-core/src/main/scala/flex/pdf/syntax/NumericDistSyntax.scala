package flex.pdf.syntax

import flex.pdf.NumericDist

trait NumericDistSyntax extends NumericDistPropSyntax

trait NumericDistPropSyntax {

  implicit class NumericDistPropSyntaxImpl[A](dist: NumericDist[A]) {}

}
