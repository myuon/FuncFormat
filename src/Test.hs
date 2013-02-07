{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import StrFormat
import Control.Lens

import Debug.Trace

-- [format| {_1}|] f (a,b) == show $ f _1 (a,b)

main = putStrLn $ [format| {_1}!|] (\x (y,z) -> set x y z) ("hello", (10,100))
-- (hello,100)!

-- main = putStrLn $ [format| {_1}!|] (\x y -> view x y) ("hello", (10,100))
-- hello!

