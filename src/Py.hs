{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Py
    ( param2param
    , tupleParam
    , func2func
    -- , unCamel
    -- , toUnixStyle
    , isArray
    , pPyType
    ) where

import Prettyprinter
import Data.Char
import qualified Data.Text as T
import qualified Func as F
import HType (HType (..))
import qualified HType as HDATA
import Data.Function
import Data.Foldable
import Data.List (intercalate)


--------------------- data define-------------------------------------

-- | used for trans to ctypes before pass to dll
data Ctype
    = Byref
    | CCharP
    | CInt
    | CFloat
    | NDInt
    | NDfloat
    | Null
    deriving (Show,Eq)

data Param = Param
    { paramName :: T.Text
    , paramType :: T.Text
    , paramCtype :: Ctype}
    deriving (Show)

data Func = Func
    { fName :: T.Text
    , fType :: Maybe T.Text
    , fDoc :: Desc
    , fParams :: [Param]
    , fBody :: [T.Text]}
    deriving (Show)

data Desc = Desc
    { descBrief :: T.Text
    , descFuncName :: T.Text
    , descArgNames :: [T.Text]
    , descArgDescs :: [T.Text]
    , descReturn :: [T.Text]}
    deriving (Show)

instance Pretty Desc where
    pretty Desc{..} = vsep [header,",",pretty descBrief,"",args,"",returns,tail]
      where
        header = "\"\"\"Wrapper for" <+> pretty descArgNames
        args = nest 4 $ "Args:" <> line <>  vsep (zipWith makeArg descArgNames descArgDescs)
        makeArg name desc = pretty name <> ": " <> align (pretty desc)
        returns = nest 4 $ "Returns:" <> line <> vsep ( fmap (hang 4 . pretty) descReturn)
        tail = "\"\"\""

instance Pretty Param where
    pretty Param{..} = pretty (paramName <> ":") <+>  pretty paramType

instance Pretty Func where
    pretty Func{..} = vsep [header, indent 4 $ pretty fDoc,"", nest 4 . vsep $ fmap pretty fBody]
      where
        header = "def" <+> hang 4 (pretty fName <> tupleParam fParams<> anno)
        anno = case fType of
                Nothing -> ":"
                Just t -> "\\" <> line <> "->" <+> pretty t <> ":"


---------------------------- transfroming functions ------------------------
param2param :: F.Param -> Param
param2param p@F.Param{..} = uncurry (Param pName) pType
  where
    pName = paramName
    pType = pPyType p

func2func :: F.Func -> Func
func2func f@F.Func{..}= Func{..}
  where
    fName = T.pack . toUnixStyle . unCamel . drop 5 . T.unpack $ funcName
    fType = getType f
    fBody = getBody f
    fDoc = getDoc f
    fParams = fmap param2param funcInParams


------------------------------helper functions ---------------------------------

-- | pretty [Param]
tupleParam :: [Param] -> Doc ann
tupleParam [] = "()"
tupleParam ps = "("  <>  (align .  vsep)  ds
  where
    ds = zipWith (<>) (map pretty ps) (replicate (length ps - 1) "," <> [")"])

-- | trans a Func.Param to python type
pPyType :: F.Param -> (T.Text,Ctype)
pPyType p@F.Param{..} = case paramType of
    Point (Const Session)
        -> ("HDATA.Session", Byref)
    Point (Const Char) -- ^ const char * -> str
        -> ("str", CCharP)
    Point (Const ht)
        -> ("npt.NDArray[" <> fst (hType2pType ht) <> "]",ndf $ snd $ hType2pType ht)
    Point ht
        -> if isArray p
           then ("Array", snd $ hType2pType ht) -- ^ Array, should be allocated beform pass to function
           else (fst $ hType2pType ht, Byref)
    Const _ -> error "unexpected Const _"
    ht -> hType2pType ht
  where
    ndf = \case
        CInt -> NDInt
        CFloat -> NDfloat
        ct -> error $ "nd don't expect " <> show ct

-- | helper for trans HType to pytho type text, not recursive
hType2pType :: HType -> (T.Text,Ctype)
hType2pType = \case
    DECL -> ("", Null)
    Session -> error "unexpected session"
    StringHandle -> ("int", CInt)
    PDG_GraphContextId -> ("int", CInt)
    Int -> ("int", CInt)
    NodeId -> ("int", CInt)
    Char -> error "python have no char"
    PDG_WorkItemId -> ("int", CInt)
    PDG_EventInfo -> ("HDATA.PDG_EventInfo", Null)
    PDG_WorkItemInfo -> ("HDATA.PDG_WorkItemInfo", Null)
    Float -> ("float", CFloat)
    ht -> error $ "unexpected HType" <> show ht

-- | judge if param is a array of something (example: int * -> [c_int])
isArray :: F.Param -> Bool
isArray F.Param{..} = "array" `T.isInfixOf` paramName

getDoc :: F.Func -> Desc
getDoc F.Func{..}= Desc{..}
  where
    descBrief = funcDesc
    descArgNames = fmap F.paramName funcInParams
    descArgDescs = fmap F.paramDesc funcInParams
    descReturn = fmap F.paramDesc funcOutParams
    descFuncName = funcName

-- | from Func.Func get a python body
getBody :: F.Func -> [T.Text]
getBody F.Func{..}= [bNDInt,bNDfloat] <> bsPreArray <> [bResult,bAssert,bResurn]
  where
    inParams = fmap param2param funcInParams
    returns = fmap param2param funcOutParams
    params = fmap param2param funcParams
    inCtypes = [ paramCtype x | x <- inParams]
    -- | pre ndarray[int]
    bNDInt =
        if NDInt `elem` inCtypes
        then "intP = POINTER(c_int)" :: T.Text
        else ""
    -- | preprocess of ndarray[float]
    bNDfloat =
        if NDfloat `elem` inCtypes
        then "floatP = POINTER(c_float)" :: T.Text
        else ""
    -- | preprocess of Array
    bsPreArray = foldr preArrayHelper [] returns
    preArrayHelper p@Param{..} bs =
        if "Array" == paramType
        then paramName
            <> " = ("
            <> (paramCtype & (\case
                    CInt -> "c_int"
                    CFloat -> "c_float"
                    _ -> error "unsupported type in array"))
            <> " * length)()" : bs
        else bs
    argTexts = fmap param2Arg params
    bResult = "result = HAPI_LAB." <> funcName <>"(" <> T.intercalate ", " argTexts <>")"
    bAssert = "assert HDATA.Result.SUCCESS == result, '"
        <> T.drop 5 funcName
        <> " Failed with {0}'.format(HDATA.Result(result).name)"
    bResurn =
        if null returns
        then ""
        else "return "
            <> T.intercalate ", " ( fmap paramName returns)

getType :: F.Func -> Maybe T.Text
getType F.Func{..} =
    case funcOutParams of
        [] -> Nothing
        [x] -> Just . fst $ pPyType x
        xs -> Just $ "Tuple[" <> T.intercalate ", " (fmap (fst . pPyType) xs) <> "]"

param2Arg :: Param -> T.Text
param2Arg p@(paramType -> "Array") = "byref(" <> paramName p <> ")"
param2Arg p@(paramCtype -> NDInt) = paramName p <> "flatten().ctypes.data_as(intP)"
param2Arg p@(paramCtype -> NDfloat) = paramName p <> "flatten().ctypes.data_as(floatP)"
param2Arg p@(paramCtype -> Byref) = "byref(" <> paramName p <> ")"
param2Arg p@(paramCtype -> CInt) = "c_int(" <> paramName p <> ")"
param2Arg p@(paramCtype -> CFloat) = "c_float(" <> paramName p <> ")"
param2Arg p@(paramCtype -> CCharP) = "c_char_p(" <> paramName p <> ".encode('utf-8'))"
param2Arg p@(paramCtype -> Null) = paramName p
param2Arg _ = error "unsupported param"

unCamel :: String -> [String]
unCamel a =filter (not . null) $ (\ (a, b) -> a <> [b]) $ foldl' f ([],[]) a
  where
    f (as,b) c=
        if isUpper c
        then (as ++ [b], [c])
        else (as, b <> [c])

toUnixStyle :: [String] -> String
toUnixStyle = map toLower . intercalate "_"