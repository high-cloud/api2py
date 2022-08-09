module Lib
    ( someFunc
    , te1
    ) where


import Py
import XMLLayer
import Text.XML.Light (strContent, ppElement)
import Prettyprinter
import Func (Func(..))
import Prettyprinter.Util (putDocW)
import Prettyprinter.Render.Text (renderLazy, renderStrict)
import qualified Data.Text as T
import Data.List (intercalate)


someFunc = do
    file <- readFile "pdg.xml"
    let fileName = "te.py"
    let fs = getFunc file
    let option = LayoutOptions {layoutPageWidth = AvailablePerLine 80 1.0}
    let f's =  fs
    let helper = T.unpack . renderStrict  . layoutPretty option . pretty . func2func
    writeFile fileName $ intercalate "\n\n\n" (fmap helper f's)

te1 :: Int -> Int -> IO ()
te1 n m = do
    file <- readFile "pdg.xml"
    let fs = getFunc file
    print $  pPyType $ funcOutParams (fs !! n) !! m