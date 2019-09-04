package flex.conf

import flex.SketchConf
import flex.conf.pdf.{ DefaultDataBinningDistConf, SmoothDistConf }

trait ConfPkgSyntax extends ConfPkgSyntax1

trait ConfPkgSyntax1 extends ConfPkgSyntax2 {

  implicit val defaultSmoothDistConf: flex.conf.pdf.SmoothDistConf = SmoothDistConf.default

}

trait ConfPkgSyntax2 extends ConfPkgSyntax3 {

  implicit val defaultSketchConf: flex.conf.pdf.SketchConf = SketchConf.default

}

trait ConfPkgSyntax3 {

  implicit val defaultDataBinningDistConf: flex.conf.pdf.DataBinningDistConf = DefaultDataBinningDistConf

}
