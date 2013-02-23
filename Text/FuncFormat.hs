{-# LANGUAGE TemplateHaskell #-}
module Text.FuncFormat where
import Language.Haskell.TH.Quote

import Data.Text (pack)
import Text.FuncFormat.Internal

format :: QuasiQuoter
format = QuasiQuoter { quoteExp = parseExp . pack
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined
                     }


