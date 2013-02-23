{-# LANGUAGE TemplateHaskell #-}
module Data.Text.FuncFormat.Internal
       ( parseExp
       , noEscaped
       ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos (updatePosChar)
import Text.Parsec.Text
import Control.Monad (replicateM)

import Control.Applicative hiding (many)

import qualified Data.Text as T

type FString = String
data Format = Normal String
            | Func String
            deriving (Show)

isFunc :: Format -> Bool
isFunc (Func _) = True
isFunc _ = False

formatExpr :: Parser [Format]
formatExpr = many1 (try fmFunc <|> fmString)

fmFunc :: Parser Format
fmFunc = Func <$> do
  char '{'
  spaces
  name <- many1 $ noneOf "} "
  spaces
  char '}'
  return name

fmString :: Parser Format
fmString = Normal <$> (many1 $ noneOf "{}")

noEscaped :: T.Text -> T.Text
noEscaped s = case parse prettyStr "" s of
  Left err -> s
  Right x -> x
  
  where
    prettyStr :: Parser T.Text
    prettyStr = T.pack <$> do
      char '\"'
      content <- many1 $ noneOf "\""
      char '\"'
      return content

parseExp :: T.Text -> ExpQ
parseExp s = case parse formatExpr "" s of
  Left err -> error $ show err
  Right xs ->
    case any isFunc xs of
      True -> do
        nx <- newName "x"
        nfs <- replicateM (length $ filter isFunc xs) $ newName "f"
        let fm = foldr appE [| "" |] $ buildExp nx nfs xs
        lamE (map varP nfs ++ [varP nx]) fm
      False -> [| T.pack $(litE $ stringL $ T.unpack s) |]

buildExp :: Name -> [Name] -> [Format] -> [ExpQ]
buildExp nx ff@(f:fs) (x:xs) = let (e, isF) = toExpQ x f nx in
  if isF then e:(buildExp nx fs xs)
  else e:(buildExp nx ff xs)
buildExp _ [] xs = map (\x -> fst $ toExpQ x 'undefined 'undefined) xs
buildExp _ _ _ = error "Couldn't match the number of functions"

toExpQ :: Format -> Name -> Name -> (ExpQ, Bool)
toExpQ format nf nx =
  case format of
    Func f -> ([| T.append $ noEscaped . T.pack . show $ $(varE nf) $(varE $ mkName f) $(varE nx) |], True)
    Normal f -> ([| T.append $ T.pack $(litE $ stringL f) |], False)
    
