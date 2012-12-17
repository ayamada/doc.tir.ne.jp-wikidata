module ParentLink (plugin) where

import Network.Gitit.Interface
import Network.URL
import Text.StringTemplate
import Text.Html (stringToHtmlString)
import Data.List as L
import qualified Data.Text as T
import Control.Monad
import Data.FileStore
import Data.Maybe
import Text.XHtml hiding ( (</>), dir, method, password, rev )

plugin :: Plugin
plugin = HSTMPTransform replaceTags

getPathPlug :: PluginM String
getPathPlug = liftM (fromJust . decString True . L.intercalate "/" . rqPaths) askRequest

getWikiPath :: PluginM String
getWikiPath = do
  path <- getPathPlug
  return $ if null path then "" else '/':path

mkTargetPathList (x:xs) = (:) [x] $ map (\v -> x:v) $ mkTargetPathList xs
mkTargetPathList [] = []

getUrl path = do
  let urlTrue = concat path
  let pathTrue = tail $ if (last path) == "/"
                          then urlTrue
                          else urlTrue ++ ".page"
  if (last path) == "/"
    then return $ Just urlTrue
    else do
      fs <- askFileStore
      hist <- liftIO $ history fs [pathTrue] (TimeRange Nothing Nothing) $ Just 1
      return $ if null hist then Nothing else Just urlTrue

mkUrlList pathList = mapM getUrl pathList

replaceParentLink :: PluginM String
replaceParentLink = do
  wikiPath <- getWikiPath
  let splitted = map T.unpack $ if null wikiPath then [] else T.split (=='/') $ T.pack wikiPath
  let tail' l = if null l then [] else tail l
  let targetPathList = mkTargetPathList $ tail' $ intersperse "/" splitted
  urlList <- mkUrlList targetPathList
  let targetLabelList = map last targetPathList
  let labelAndUrlList = zip targetLabelList urlList
  let labelAndUrlListToHtml (label, Nothing) = thespan <<
        [ thespan ! [theclass "underline"] << toHtml (" " ++ label ++ " ") ]
      labelAndUrlListToHtml (label, Just url) = thespan <<
        [ anchor ! [href url] << toHtml (" " ++ label ++ " ") ]
  let parentLinkHtml = intersperse (toHtml " ") $ map labelAndUrlListToHtml labelAndUrlList
  let resultHtml = if null labelAndUrlList
        then noHtml
        else thediv ! [theclass "parentlink"] << ([toHtml "["] ++ parentLinkHtml ++ [toHtml "]"])
  return $ renderHtmlFragment resultHtml


replaceTags :: StringTemplate String -> PluginM (StringTemplate String)
replaceTags tmpl = do
  rpl <- replaceParentLink
  return $ setAttribute "parentlink" rpl tmpl

