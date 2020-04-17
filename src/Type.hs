module Type where

import Data.ByteString
import Data.Word8

newtype Markdown = Markdown [MDElem]
  deriving (Show)

data MDElem = Header1             ByteString
            | Header2             ByteString
            | Header3             ByteString
            | Header4             ByteString
            | Header5             ByteString
            | Header6             ByteString
            | Header7             ByteString
            | Paragrah            [MDElem]    -- Should contain Italic, Bold, BoldAndItalic, Code
                                              --                                    , Link, Image
            | PlainText           ByteString
            | Italic              ByteString
            | Bold                ByteString
            | BoldAndItalic       ByteString
            | Code                ByteString
            | CodeBlock           ByteString
            | Link                ByteString ByteString ByteString
            | Image               ByteString ByteString ByteString
            | OrderedListElem     ByteString
            | UnorderedListElem   ByteString
            | Blockquotes         [MDElem]
  deriving (Show)
