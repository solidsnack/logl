import Network.Wai.Handler.Warp

import Language.LogL.Backend
import Language.LogL.Server


main                         =  do
  stdout :: STDOUT          <-  start ()
  serve 0xFF stdout defaultSettings Nothing


