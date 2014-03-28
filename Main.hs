module Main where

import Control.Monad
import Happstack.Server (Browsing(DisableBrowsing), nullConf, simpleHTTP,
                         dir, serveDirectory)
import Templates

-- | main.
main :: IO ()
main = simpleHTTP nullConf $ msum [ dir "USD" $ curlBTCUSD
                                  , dir "static" $ serveDirectory DisableBrowsing [] "static"
                                  , homepage
                                  ]
