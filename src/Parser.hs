{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator
import Data.ByteString (ByteString, pack, singleton)
import Data.Word8
import Control.Applicative
import Type

mdElem :: Parser MDElem
mdElem = italic <|> bold <|> code <|> codeBlock <|> header <|> para

para :: Parser MDElem
para = PlainText <$> do
  w <- anyWord8
  text <- takeTill (inClass "![_*>`")
  return (singleton w <> text)

italic :: Parser MDElem
italic = do
  m <- satisfy isAstrOrUds
  _ <- lookAhead (satisfy (not <$> isAstrOrUds))
  text <- takeTill isAstrOrUds
  _ <- satisfy (== m)
  return (Italic text)

bold :: Parser MDElem
bold = do
  m <- count 2 $ satisfy isAstrOrUds
  _ <- lookAhead (satisfy (not <$> isAstrOrUds))
  text <- takeTill isAstrOrUds
  _ <- string $ pack.reverse $ m
  return (Bold text)

code :: Parser MDElem
code = do
  _ <- word8 96
  text <- takeWhile1 (/= 96)
  _ <- word8 96
  return (Code text)

codeBlock :: Parser MDElem
codeBlock = do
  _ <- string "```\n"
  t1 <- takeTill (== 96)
  checkEnd t1
  where checkEnd t1 = do
          t2 <- takeTill (== 96)
          s <- string "```" <|> takeWhile1 (== 96)
          if s == "```"
             then return (CodeBlock $ t1 <> t2)
             else checkEnd (t1 <> s)

header :: Parser MDElem
header = h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6 <|> h7

h1 :: Parser MDElem
h1 = Header1 <$> header' 1

h2 :: Parser MDElem
h2 = Header2 <$> header' 2

h3 :: Parser MDElem
h3 = Header3 <$> header' 3

h4 :: Parser MDElem
h4 = Header4 <$> header' 4

h5 :: Parser MDElem
h5 = Header5 <$> header' 5

h6 :: Parser MDElem
h6 = Header6 <$> header' 6

h7 :: Parser MDElem
h7 = Header7 <$> header' 7

header' :: Int -> Parser ByteString
header' i = do
  _ <- count i (word8 35)
  _ <- lookAhead (notWord8 35)
  _ <- many (word8 32)
  takeTill isEndOfLine

isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 10 || w == 13

isAstrOrUds :: Word8 -> Bool
isAstrOrUds w = w == 95 || w == 42
