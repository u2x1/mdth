module Type where

import Data.ByteString
import Data.Word8

data MDElem = Blockquotes [MDElem]
            | Header1 ByteString
            | Header2 ByteString
            | Header3 ByteString
            | Header4 ByteString
            | Header5 ByteString
            | Header6 ByteString
            | Header7 ByteString
            | Italic          ByteString
            | Bold            ByteString
            | BoldAndItalic   ByteString
            | Code            ByteString
            | Link            ByteString ByteString ByteString
            | Image           ByteString ByteString ByteString
            | PlainText       ByteString
            | CodeBlock ByteString
            | OrderedList [ByteString] | UnorderedList [ByteString]
  deriving (Show)
