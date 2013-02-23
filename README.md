FuncFormat
======================

Want to format like `string.format` in Python in Haskell program? Use this!

Sample
-----------------
Here is a tiny sample.

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.FuncFormat
import Data.Text
import Control.Lens

-- [format| {_1} |] f (a,b) == pack $ show $ f _1 (a,b)

main = putStrLn $ unpack $ [format| {_1}, {_2}!|] view view ("hello", "world")
-- output: hello, world!
```

I strongly recommend you import `Control.Lens` ([Lens package](http://hackage.haskell.org/package/lens )) so that this `format` function can be easy to handle.

**Be careful!**
Do not forget to declare GHC extensions `{-# LANGUAGE QuasiQuotes #-}` and `{-# LANGUAGE OverloadedStrings #-}` at first line in your .hs file.

How does it works
-----------------
There is only one expression to use this `format`:  
```haskell
[format| {f1} {f2}|] g1 g2 t == pack $ show (f1 g1 t) ++ show (f2 g2 t)
```
`format` returns Text (from [Data.Text](http://hackage.haskell.org/package/text))

Then, if you just want to show the elements of a tuple:  
```haskell
[format| {_1}|] view (1,2)
-- output: 1
```  
(`view` and `_1` come from [Control.Lens.Getter](http://hackage.haskell.org/packages/archive/lens/3.8.5/doc/html/Control-Lens-Getter.html ))

If you want to show the elements of a tuple but after applying some other function:  
```haskell
[format| {_1}!|] (\x (y,z) -> set x y z) ("hello", (10,100))
-- output: (hello,100)!
```

There must be some other way to use `format`. Anyways, this may be helpful when you make a pretty printer by yourself :)

