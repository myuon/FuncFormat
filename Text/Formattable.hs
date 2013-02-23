{-# LANGUAGE FlexibleInstances #-}
module Text.Formattable where

import qualified Data.Text as T

class Formattable f where
  pack :: T.Text -> f
  unpack :: f -> T.Text
  
instance Formattable T.Text where
  pack = id
  unpack = id

instance Formattable Int where
  pack = read . T.unpack
  unpack = T.pack . show

instance Formattable Double where
  pack = read . T.unpack
  unpack = T.pack . show

instance Formattable Float where
  pack = read . T.unpack
  unpack = T.pack . show

instance Formattable Char where
  pack = read . T.unpack
  unpack = T.pack . show

instance Formattable String where
  pack = T.unpack
  unpack = T.pack

