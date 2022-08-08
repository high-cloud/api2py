module Lib
    ( someFunc
    ) where


import Py
import XMLLayer
import Text.XML.Light (strContent, ppElement)
import Prettyprinter
import Func (Func(funcParams))

someFunc :: Int -> IO ()
someFunc n= do
    file <- readFile "pdg.xml"
    let fs = getFunc file
    print $  tupleParam $ map param2param  (funcParams (fs !! n))

