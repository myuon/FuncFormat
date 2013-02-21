{-# LANGUAGE TemplateHaskell #-}
module Data.Text.FuncFormat where
import Language.Haskell.TH.Quote

import Data.Text (pack)
import Data.Text.FuncFormat.Internal

format :: QuasiQuoter
format = QuasiQuoter { quoteExp = parseExp . pack
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined
                     }


