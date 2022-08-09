{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module XMLLayer where

import Func
import HType
import Text.XML.Light
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Text.XML.Light.Lexer




getFunc file =
    map ele2func fs
  where
    xml = fromJust $ parseXMLDoc file
    fs = findElements (makeQName "memberdef") xml

--------------------- some helper function with xml

makeQName :: String -> QName
makeQName s = QName s Nothing Nothing

findElement' :: QName -> Element -> Element
findElement' qn e = fromJust $ findElement qn e


findChild' :: QName -> Element -> Element
findChild' qn e = fromJust $ findChild qn e

strContent' :: Element -> T.Text
strContent' = T.pack . strContent

-- | recursion version of strContent
strContentRec :: Element -> T.Text
strContentRec = T.unwords . strsContentRec

-- | recursion version of get str from element and words it
strsContentRec :: Element -> [T.Text]
strsContentRec e = T.words . T.concat . concatMap f $ elContent e
  where
    p = findChild (makeQName "para") e
    f :: Content -> [T.Text]
    f = \case
        Text d -> [T.pack $ cdData d]
        Elem e ->  concatMap f $ elContent e
        _ -> error "expect no CRef"

-- | get the text from briefdescription element, todo: trim space before period
briefDesc2Text :: Element -> T.Text
briefDesc2Text e = T.unwords . T.words . T.concat . concatMap f $ elContent e
  where
    p = findChild (makeQName "para") e
    f :: Content -> [T.Text]
    f = \case
        Text d -> [T.pack $ cdData d]
        Elem e ->  concatMap f $ elContent e
        _ -> error "expect no CRef"


-- | get a Func from <memberdef>
ele2func e =
    Func{..}
  where
    funcName = strContent' $ findElement' (makeQName "name") e
    funcType = texts2HType . T.words . strContent' $ findElement' (makeQName "type") e
    params = findElements (makeQName "param") e
    funcDesc = strContentRec $ findElement' (makeQName "briefdescription") e
    parameteritems = findElements (makeQName "parameteritem") e
    funcParams = zipWith getParam params parameteritems
    (funcInParams, funcOutParams) = splitParams funcParams


---------------------  param information extract

getParamType
    :: Element -- ^ <param>
    -> HType
getParamType = texts2HType . strsContentRec . findChild' (makeQName "type")

getParamName
    :: Element -- ^ <param>
    -> T.Text
getParamName = strContent' . findChild' (makeQName "declname")

getParamDir
    :: Element -- ^ <parameteritem>
    -> Direction
getParamDir e =
    case s of
        "in" -> In
        "out" -> Out
        _ -> error "getParamDir: unexpected str"
  where
    s = fromJust . findAttr (makeQName "direction") . findElement' (makeQName "parametername") $ e

getParamDesc
    :: Element -- ^ <paramteritem>
    -> T.Text
getParamDesc = strContentRec . findElement' (makeQName "parameterdescription")

-- | get a Param from a <param> and <paramteritem>
getParam :: Element -> Element -> Param
getParam p d =
    Param{..}
  where
    paramType = getParamType p
    paramName = getParamName p
    paramDesc = getParamDesc d
    paramDir = getParamDir d

