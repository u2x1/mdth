{-# LANGUAGE OverloadedStrings #-}
module Convert where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List (intersperse)
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
convertMD' HorizontalRule = "<hr>"
convertMD' (Paragrah xs) = addElemTag "p" $ mconcat $ convertMD' <$> xs
convertMD' (Blockquotes xs) = addElemTag "blockquote" $ mconcat $ convertMD' <$> xs
convertMD' (OrderedList xs) = addElemTag "ol" $ mconcat $ convertMD' <$> xs
convertMD' (UnorderedList xs) = addElemTag "ul" $ mconcat $ convertMD' <$> xs
convertMD' (OrderedListElem x) = addElemTag "li" x
convertMD' (UnorderedListElem x) = addElemTag "li" x
convertMD' (PlainText x) = x
convertMD' (Italic x) = addElemTag "em" x
convertMD' (Bold x) = addElemTag "strong" x
convertMD' (BoldAndItalic x) = addElemTag "strong" $ addElemTag "em" x
convertMD' (Strikethrough x) = addElemTag "s" x
convertMD' (Link text url title) = addElemTagwithProp "a" [("href", Just url), ("title", title)] text
convertMD' (Image text url title) = addElemTagwithProp "img" [("src", Just url), ("alt", Just text), ("title", title)] ""
convertMD' (Code x) = addElemTag "code" x
convertMD' (CodeBlock x) = addElemTag "pre" $ addElemTag "code" x

addElemTag :: ByteString -> ByteString -> ByteString
addElemTag tag x = mconcat ["<", tag, ">\n", x, "\n</", tag, ">\n"]

addElemTagwithProp :: ByteString -> [(ByteString, Maybe ByteString)] -> ByteString -> ByteString
addElemTagwithProp tag prop x = mconcat ["<", tag, mconcat.intersperse " " $ makeProp <$> prop, ">\n", x, "\n</", tag, ">\n"]
  where
    makeProp (_, Nothing) = ""
    makeProp (name, (Just value)) = mconcat [name, "=\"", value, "\""]
