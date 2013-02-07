{-# LANGUAGE TemplateHaskell #-}
module StrFormat where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Control.Lens

import Internal

format :: QuasiQuoter
format = QuasiQuoter { quoteExp = parseExp
                     , quotePat = undefined
                     , quoteType = undefined
                     , quoteDec = undefined
                     }
