package flip.conf

import flip.SketchConf
import flip.conf.pdf.{DefaultDataBinningDistConf, SmoothDistConf}

trait ConfPkgSyntax extends ConfPkgSyntax1

trait ConfPkgSyntax1 extends ConfPkgSyntax2 {

  implicit val defaultSmoothDistConf: flip.conf.pdf.SmoothDistConf = SmoothDistConf.default

}

trait ConfPkgSyntax2 extends ConfPkgSyntax3 {

  implicit val defaultSketchConf: flip.conf.pdf.SketchConf = SketchConf.default

}

trait ConfPkgSyntax3 {

  implicit val defaultDataBinningDistConf: flip.conf.pdf.DataBinningDistConf = DefaultDataBinningDistConf

}
