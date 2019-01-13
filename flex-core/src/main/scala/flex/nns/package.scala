package flex

import flex.pdf.VQH
import org.nd4j.linalg.api.ndarray.INDArray

package object nns {

  type NDArrayLSH = LSH[INDArray]

  type CodewordLSH = LSH[VQH#Codeword]

  type NDArrayANN = ANN[INDArray]

  type CodewordANN = ANN[VQH#Codeword]

  object syntax extends ANNSyntax with ParANNSyntax with LSHSyntax

}
