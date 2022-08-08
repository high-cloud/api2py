{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Py
    ( param2param
    , tupleParam
    ) where

import Prettyprinter
import qualified Data.Text as T
import qualified Func as F
import HType (HType (..))
import qualified HType as HDATA


--------------------- data define-------------------------------------
data Param = Param
    { paramName :: T.Text
    , paramType :: T.Text}

data Func = Func
    { fName :: T.Text
    , fType :: Maybe T.Text
    , fDoc :: Desc
    , fParams :: [Param]
    , fBody :: [T.Text]}

data Desc = Desc
    { descBrief :: T.Text
    , descArgs :: [T.Text]
    , descReturn :: T.Text}

instance Pretty Param where
    pretty Param{..} = pretty (paramName <> ":") <+>  pretty paramType

instance Pretty Func where
    pretty Func{..} = pretty fName

---------------------------- transfroming functions ------------------------
param2param :: F.Param -> Param
param2param p@F.Param{..} = Param pName pType
  where
    pName = paramName
    pType = pPyType p

func2func :: F.Func -> Func
func2func = _
------------------------------helper functions ---------------------------------

-- | pretty [Param]
tupleParam :: [Param] -> Doc ann
tupleParam [] = "()"
tupleParam ps = "("  <>  (align .  vsep)  ds
  where
    ds = zipWith (<>) (map pretty ps) (replicate (length ps - 1) "," <> [")"])

-- | trans a Func.Param to python type
pPyType :: F.Param -> T.Text
pPyType p@F.Param{..} = case paramType of
    Point (Const Session)
        -> "HDATA.Session"
    Point (Const Char) -- ^ const char * -> str
        -> "str"
    Point (Const ht)
        -> "npt.NDArray[" <> hType2pType ht <> "]"
    Point ht
        -> if isArray p
           then "Array"
           else hType2pType ht
    Const _ -> error "unexpected Const _"
    ht -> hType2pType ht

-- | helper for trans HType to pytho type text, not recursive
hType2pType :: HType -> T.Text
hType2pType = \case
    DECL -> ""
    Session -> "HDATA.Session"
    StringHandle -> "int"
    PDG_GraphContextId -> "int"
    Int -> "int"
    NodeId -> "int"
    Char -> error "python have no char"
    PDG_WorkItemId -> "int"
    PDG_EventInfo -> "HDATA.PDG_EventInfo"
    PDG_WorkItemInfo -> "HDATA.PDG_WorkItemInfo"
    Float -> "float"
    _ -> "unexpected HType"

-- | judge if param is a array of something (example: int * -> [c_int])
isArray :: F.Param -> Bool
isArray F.Param{..} = "array" `T.isInfixOf` paramName

getDoc :: F.Func -> Desc
getDoc = _

getBody :: F.Func -> T.Text
getBody = _