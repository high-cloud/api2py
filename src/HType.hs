{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}


module HType
    ( HType(..)
    , texts2HType
    ) where

import qualified Data.Text as T

-- | HAPI types
data HType
    = DECL
    | Session
    | StringHandle
    | PDG_GraphContextId
    | Int
    | NodeId
    | Char
    | PDG_WorkItemId
    | PDG_EventInfo -- todo
    | PDG_WorkItemInfo -- ^ todo
    | PDG_WorkItemOutputFile
    | Float
    | Bool
    | Pointer HType
    | Const HType
    deriving (Show)


-- | helper function for transform a text to HType
texts2HType :: [T.Text] -> HType
texts2HType [] = DECL
texts2HType ["HAPI_DECL"] = DECL
texts2HType ["HAPI_Session"] = Session
texts2HType ["HAPI_StringHandle"] = StringHandle
texts2HType ["HAPI_PDG_GraphContextId"] = PDG_GraphContextId
texts2HType ["int"] = Int
texts2HType ["float"] = Float
texts2HType ["HAPI_Bool"] = Bool
texts2HType ["HAPI_NodeId"] = NodeId
texts2HType ["char"] = Char
texts2HType ["HAPI_PDG_WorkItemId"] = PDG_WorkItemId
texts2HType ["HAPI_PDG_WorkItemInfo"] = PDG_WorkItemInfo
texts2HType ["HAPI_PDG_EventInfo"] = PDG_EventInfo
texts2HType ["HAPI_PDG_WorkItemOutputFile"] = PDG_WorkItemOutputFile
texts2HType ts@(last -> "*") = Pointer $ texts2HType $ init ts
texts2HType ("const":ts) = Const . texts2HType $ ts
texts2HType x = error $  "texts2HType: unexpect pattern" <> show x

    