package flip.pdf.syntax

import flip.pdf.NumericDist

trait NumericDistSyntax extends NumericDistPropSyntax

trait NumericDistPropSyntax {

  implicit class NumericDistPropSyntaxImpl[A](dist: NumericDist[A]) {}

}
