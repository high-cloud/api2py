module Func
    ( Param(..)
    , Func(..)
    , Direction(..)
    , splitParams
    ) where

import qualified Data.Text as T
import HType
import Data.Foldable (Foldable(fold))

-- | indicate parameter is input or output
data Direction = In | Out
    deriving (Show)

-- | metadata for parameter in func
data Param = Param
    { paramDir :: Direction
    , paramType :: HType
    , paramName :: T.Text
    , paramDesc :: T.Text
    }
    deriving (Show)

-- | metadata for function
data Func = Func
    { funcType :: HType
    , funcName :: T.Text
    , funcParams :: [Param]
    , funcInParams :: [Param]
    , funcOutParams :: [Param]
    , funcDesc :: T.Text}
    deriving (Show)

splitParams :: [Param] -> ([Param],[Param])
splitParams = foldr f ([],[])
  where
    f p (is,os) = case paramDir p of
        In -> (p:is,os)
        Out -> (is, p:os)
