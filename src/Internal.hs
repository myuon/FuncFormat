{-# LANGUAGE TemplateHaskell #-}
module Internal(parseExp) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>))
import Control.Lens

import Control.Applicative hiding (many)

-- [format| {_1}, {_2}! |] $. ("hello","world") --> hello, world!

type FString = String
data Format = Normal String
            | Getter String
            | Func String
            deriving (Show)

isFunc :: Format -> Bool
isFunc (Func _) = True
isFunc (Getter _) = True
isFunc _ = False

skipSpace :: Parsec String () a -> Parsec String () a
skipSpace p = skipMany space *> p

elimEsc :: String -> String
elimEsc = elim "\"'"
  where
    elim :: (Eq a) => [a] -> [a] -> [a]
    elim cs = filter (\x -> not $ x `elem` cs)

formatExpr :: Parsec String () [Format]
formatExpr = many1 (try fmGetter <|> try fmFunc <|> fmString)

symbol :: String -> Parsec String () String
symbol = skipSpace . string

fmGetter :: Parsec String () Format
fmGetter = Getter <$> between (symbol "{") (symbol "}") (many1 $ noneOf "}")

fmFunc :: Parsec String () Format
fmFunc = Func <$> between (symbol "<") (symbol ">") (many1 $ noneOf ">")

fmString :: Parsec String () Format
fmString = Normal <$> (many1 $ noneOf "{}<>")

parseExp :: String -> ExpQ
parseExp s = case parse formatExpr "" s of
  Left err -> [| putStrLn $(litE $ stringL (show err)) |]
  Right x ->
    case any isFunc x of
      True -> do
        nx <- newName "x"
        nf <- newName "f"
        let fm = foldr appE [| "" |] $ map (\f -> toExpQ f nf nx) $ x
        lamE [varP nf, varP nx] fm
      False -> [| print $(litE $ stringL s) |]

toExpQ :: Format -> Name -> Name -> ExpQ
toExpQ format nf nx =
  case format of
    Func f -> [| (++) $ elimEsc . show $ $(varE $ mkName f) $(varE nx) |]
    Getter f -> [| (++) $ elimEsc . show $ $(varE nf) $(varE $ mkName f) $(varE nx) |]
    Normal f -> [| (++) $(litE $ stringL f) |]
    
