
import Prelude  hiding (LT, EQ, NE, GT, LE, GE)

import C0AMtypes

import C0AMformat

neu = eraseNOPs [(C 1 E,NOP),(C 2 E,NOP),(C 3 E,EQ)]

alt = eraseNOPs' [(C 1 E,NOP),(C 2 E,NOP),(C 3 E,EQ)]

main = do print neu
          print alt
