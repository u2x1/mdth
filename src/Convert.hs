{-# LANGUAGE OverloadedStrings #-}
module Convert where

import Data.Attoparsec.ByteString
import Data.ByteString
import Type
import Parser

convertMD :: ByteString -> Maybe ByteString
convertMD s = case parseOnly (many' mdElem) (s <> "\n") of
                Right xs -> Just $ mconcat $ convertMD' <$> xs
                _ -> Nothing

convertMD' :: MDElem -> ByteString
convertMD' (Header1 x) = addElemTag "h1" x
convertMD' (Header2 x) = addElemTag "h2" x
convertMD' (Header3 x) = addElemTag "h3" x
convertMD' (Header4 x) = addElemTag "h4" x
convertMD' (Header5 x) = addElemTag "h5" x
convertMD' (Header6 x) = addElemTag "h6" x
convertMD' (Header7 x) = addElemTag "h7" x
convertMD' (Paragrah xs) = addElemTag "p" $ mconcat $ convertMD' <$> xs
convertMD' (PlainText x) = x
convertMD' (Italic x) = addElemTag "em" x
convertMD' (Bold x) = addElemTag "strong" x
convertMD' (BoldAndItalic x) = addElemTag "strong" $ addElemTag "em" x
convertMD' (Code x) = addElemTag "code" x
convertMD' (CodeBlock x) = addElemTag "pre" $ addElemTag "code" x

addElemTag :: ByteString -> ByteString -> ByteString
addElemTag tag x = mconcat ["<", tag, ">\n", x, "\n</", tag, ">\n"]
