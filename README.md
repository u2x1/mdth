# mdth

A tool that helps parse markdown.

## Demo

You can find the test `md` file [here](https://github.com/Nutr1t07/mdth/tree/master/test-data/test.md).

- Convert markdown to html

  ```
  λ> import Data.ByteString as BS
  λ> s <- BS.readFile "test-data/test.md"
  λ> convertMD s
  <h1>
  h1 Heading
  </h1>
  <p>

  </p>
  <h2>
  h2 Heading
  </h2>
  <p>

  ... blah blah
  ```
  An already converted html corresponding to [test.md](https://github.com/Nutr1t07/mdth/tree/master/test-data/test.md) is [converted-test.html](https://github.com/Nutr1t07/mdth/tree/master/test-data/converted-test.html)

- Convert to AST

  ```
  λ> import Data.Attoparsec.ByteString
  λ> import Data.ByteString as BS
  λ> :set -XOverloadedStrings
  λ> s <- BS.readFile "test-data/test.md"
  λ> traverse (Prelude.putStrLn.(<>"\n").show) $ fromRight $ parseOnly (many' mdElem) (s <> "\n")
  Header1 "h1 Heading"

  Paragrah []
  
  Header2 "h2 Heading"
  
  Paragrah []
  
  Header3 "h3 Heading"
  
  Paragrah []
  
  Header4 "h4 Heading"
  
  Paragrah []
  
  Header5 "h5 Heading"
  
  Paragrah []
  
  Header6 "h6 Heading"
  
  Paragrah []
  
  Header2 "Horizontal Rules"
  
  Paragrah []
  
  HorizontalRule
  
  Paragrah []
  
  HorizontalRule
  
  Paragrah []
  
  HorizontalRule
  
  Paragrah []
  
  Header2 "Emphasis"
  
  Paragrah []
  
  Paragrah [Bold "This is bold text"]
  
  Paragrah [Bold "This is bold text"]
  
  Paragrah [Italic "This is italic text"]
  
  Paragrah [Italic "This is italic text"]
  
  Paragrah [Strikethrough "Strikethrough"]
  
  Header2 "Blockquotes"
  
  Paragrah []
  
  Blockquotes [PlainText "Blockquotes can also be nested..."]
  
  Paragrah []
  
  Blockquotes [Blockquotes [PlainText "...by using additional greater-than signs right next to each other..."]]
  
  Paragrah []
  
  Blockquotes [Blockquotes [Blockquotes [PlainText "...or with spaces between arrows."]]]
  
  Paragrah []
  
  Header2 "Lists"
  
  Paragrah []
  
  Paragrah [PlainText "Unordered"]
  
  UnorderedList [OrderedListElem "Create a list by starting a line with +, -, or *",OrderedListElem "Sub-lists are made by indenting 2 spaces:",UnorderedList [OrderedListElem "Marker character change forces new list start:",UnorderedList [OrderedListElem "Ac tristique libero volutpat at",OrderedListElem "Facilisis in pretium nisl aliquet",OrderedListElem "Nulla volutpat aliquam velit"]],OrderedListElem "Very easy!"]
  
  Paragrah []
  
  Paragrah [PlainText "Ordered"]
  
  OrderedList [OrderedListElem "Lorem ipsum dolor sit amet",OrderedListElem "Consectetur adipiscing elit",OrderedListElem "Integer molestie lorem at massa"]
  
  Paragrah []
  
  Header2 "Code"
  
  Paragrah []
  
  Paragrah [PlainText "Inline ",Code "code"]
  
  Paragrah [PlainText "Block code \"fences\""]
  
  CodeBlock "\nSample text here...\n"
  
  Paragrah []
  
  Header2 "Links"
  
  Paragrah []
  
  Paragrah [Link "link text" "http://dev.nodeca.com" Nothing]
  
  Paragrah [Link "link with title" "http://nodeca.github.io/pica/demo/" (Just "title text!")]
  
  Header2 "Images"
  
  Paragrah []
  
  Paragrah [Image "Minion" "https://octodex.github.com/images/minion.png" Nothing]
  
  Paragrah [Image "Stormtroopocat" "https://octodex.github.com/images/stormtroopocat.jpg" (Just "The Stormtroopocat")]
  
  [(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]
  ```
