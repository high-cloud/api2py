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
import Data.Text (Text)

--------------------- data define-------------------------------------
data Param = Param
    { paramName :: T.Text
    , paramType :: T.Text}

data Func = Func
    { funcName :: T.Text
    , funcType :: Maybe T.Text
    , funcDoc :: T.Text
    , funcParams :: [Param]
    , funcBody :: [T.Text]}

data Desc = Desc
    { descBrief :: T.Text
    , descArgs :: [T.Text]
    , descReturn :: T.Text}

instance Pretty Param where
    pretty Param{..} = pretty (paramName <> ":") <+>  pretty paramType

instance Pretty Func where
    pretty Func{..} = pretty funcName

---------------------------- transfroming functions ------------------------
param2param :: F.Param -> Param
param2param F.Param{..} = Param pName pType
  where
    pName = paramName
    pType = pPyType paramType

------------------------------helper functions ---------------------------------

-- | pretty [Param]
tupleParam :: [Param] -> Doc ann
tupleParam [] = "()"
tupleParam ps = "("  <>  (align .  vsep)  ds
  where
    ds = zipWith (<>) (map pretty ps) (replicate (length ps - 1) "," <> [")"])

-- | 
pPyType :: F.Param -> T.Text
pPyType F.Param{..} = case paramType of
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
    Point (Const Session) -> "HDATA.Session"
    Point (Const Char) -> "str"
    Point (Const ht) -> "npt.NDArray[" <> hType2pType ht <> "]"
    Point ht -> hType2pType ht
    Const _ -> error "unexpected Const _"

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