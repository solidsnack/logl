
import Data.ByteString.Char8

import Snap.Types
import Snap.Http.Server


snap                        ::  Snap ()
snap                         =  do
  req                       <-  getRequest
  (writeBS . pack . show) req

server                       =  quickHttpServe snap

main                         =  server

