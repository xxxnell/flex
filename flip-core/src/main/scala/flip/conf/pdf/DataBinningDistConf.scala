package flip.conf.pdf

import flip.pdf.DataBinningDist

trait DataBinningDistConfB[+D <: DataBinningDist[_]] extends SamplingDistConfB[D]
