module Plugins.Display.PadText(linesPad) where

import Core.Types.DisplaySize


-- Add padding the every line
linesPad :: [String] -> DisplaySize -> [String]
linesPad [] (DisplaySize _ 0) = []

-- add an empty line
linesPad [] (DisplaySize width height) = padding width : linesPad [] (DisplaySize width (height - 1))
-- add an empty line
linesPad ([]:xs) (DisplaySize width height) = padding width : linesPad xs (DisplaySize width (height - 1))

linesPad (x:xs) (DisplaySize width height) =
    (x ++ padding paddingAmount) : linesPad xs (DisplaySize width (height - 1))
    where
        paddingAmount = width - (toInteger $ length x)


padding :: Integer -> String
padding width = (foldl (++) "" $ take (fromIntegral width) $ repeat " ")
