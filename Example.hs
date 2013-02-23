{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.FuncFormat
import Data.Text
import Control.Lens

-- [format| {_1}|] f (a,b) == show $ f _1 (a,b)

main = putStrLn $ unpack $ [format|Embedded: { _1 } Message: { _2 }|] view view ("hello", 100)
-- Embedded: hello Message: 100

-- main = putStrLn $ unpack $ [format|{_1}!|] (\x (y,z) -> set x y z) ("hello", (10,100))
-- (hello,100)!

