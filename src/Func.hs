module Func
    (

    ) where

import qualified Data.Text as T

-- | indicate parameter is input or output
data Direction = In | Out

-- | metadata for parameter in func
data Parm = Parm
    { parmDir :: Direction
    , parmType :: T.Text
    , parmName :: T.Text
    , parmDecl :: T.Text
    }

-- | metadata for function
data Func = Func
    { funcType :: }
