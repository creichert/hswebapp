{-# LANGUAGE OverloadedStrings #-}
module Templates ( homepage
                 , curlBTCUSD
                 ) where

import BitcoinData

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.ByteString.Lazy

import Happstack.Server (ServerPart, Response, ok, toResponse)

import Javascript

import Language.Javascript.JMacro
import Language.Javascript.JMacro.Prelude

import Network.HTTP.Conduit (simpleHttp)

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Homepage.
homepage :: ServerPart Response
homepage = do
    r <- response $ btcTickerURL ++ "/USD"
    ok $ toResponse $ base "BitcoinAPI" [] (H.toHtml r)

-- | Curl bitcoin usd.
curlBTCUSD :: ServerPart Response
curlBTCUSD = do
    r <- response $ btcTickerURL ++ "/USD"
    ok $ toResponse $ base "Bitcoin Vaue in USD" [] $ H.toHtml r

-- | TODO: Refactor.
--
-- Two jobs done here:
--      1.) Pull and parse the json from the url into src
--      2.) Test if src is an error and if not load into BitcoinData structure.
response :: String -> ServerPart String
response url = do
    src <- (eitherDecode <$> simpleHttp url) :: ServerPart (Either String BitcoinData)
    case src of
        Left err -> return err
        Right (BitcoinData _ ask _ _ _ _) -> return $ (show ask) ++ "\n"

-- | Base html template using default headers.
base :: String -> [H.Html] -> H.Html -> H.Html
base title headers btc =
    H.html $ do
        Templates.head title headers
        body btc
        footer

head :: String -> [H.Html] -> H.Html
head title headers = H.head $ do
    H.title (H.toHtml title)
    H.script "" ! A.src "static/chart.js"
    H.script "" ! A.src "static/canvas.js"
    H.meta ! A.name "viewport"
           ! A.content "initial-scale=1, user-scalable = no"
    H.link ! A.rel "stylesheet"
           ! A.href "static/base.css"
           ! A.type_ "text/css"
    H.meta ! A.httpEquiv "Content-type"
           ! A.content   "text/html;charset=utf-8"
    sequence_ $ [H.meta ! A.name "keywords"
                        ! A.content "bitcoin, api, crypto, currency"
                ] ++ headers

header :: H.Html
header = H.header ! A.id "banner"
                  ! A.class_ "body"
                  $ do
    H.h1 $ do
        H.a "Bitcoin API" ! A.href "#"
    H.nav $ do
        H.ul $ do
            H.li ! A.class_ "active" $ do
                H.a "home" ! A.href "#"
            H.li $ do
                H.a "api" ! A.href "#"
            H.li $ do
                H.a "contact" ! A.href "#"

-- | Main body.
body :: H.Html -> H.Html
body btc = H.body ! A.id "index"
                  ! A.class_ "home"
                  $ do header
                       featured btc
                       graphing
                       extras

featured :: H.Html -> H.Html
featured btc = do
    H.aside ! A.id "featured"
            ! A.class_ "body"
            $ do
        H.hgroup $ do
            H.div ! A.id "btcpr" $ btcstr
            H.div ! A.id "btcpr" $ dllrstr
            H.div ! A.id "btcpr" $ btc
  where
    btcstr = H.toHtml ("1 BTC =" :: String)
    dllrstr = H.toHtml ("$" :: String)

-- | Implement when there is something to put in this space.
graphing :: H.Html
graphing = do
    H.canvas ! A.id "canvas"
             ! A.class_ "body"
             ! A.height "450"
             ! A.width "800"
             $ ""
    H.script (H.toHtml $ show $ renderJs $ paintGraph [] [])

extras :: H.Html
extras = H.section ! A.id "extras"
                   ! A.class_ "body" $ do
    H.div ! A.class_ "news" $ do
        H.h2 $ "news"
        H.ul $ do
            H.li $ do
                H.a "One" ! A.href "#"
                          ! A.rel "external"
            H.li $ do
                H.a "Two" ! A.href "#"
                          ! A.rel "external"
footer :: H.Html
footer = H.footer ! A.id "contentinfo"
                  ! A.class_ "body"
                  $ do
    H.address ! A.id "about"
              ! A.class_ "vcard body"
              $ do
        H.span ! A.class_ "primary"
               $ do
            H.strong $ do
                H.a "My Website" ! A.href "#"
                                 ! A.class_ "fn url"
            H.span "Bold Website Text" ! A.class_ "role"
        H.img ! A.src "images/avatar.gif"
              ! A.alt "Some logo"
              ! A.class_ "photo"
        H.span bio ! A.class_ "bio"
    H.p $ do
        H.toHtml ("2013-2014 " :: String)
        H.a "bitcoinapi.io" ! A.href "#"
  where
    bio = "Some long bio text here."
