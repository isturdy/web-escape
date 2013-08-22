{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Web.Escape (
    Escapable (..)
  ) where

import           Data.Array.Unboxed
import           Data.Char
import           Data.String
import qualified Data.Text          as T
import           Numeric

class Escapable a where
  -- | Escaping suitable for text within an HTML element
  --   (all markup within the text is converted to HTML entities).
  escapeHTML      :: a -> a
  -- | Escaping suitable for text within an HTML attribute value;
  --   prevents escape regardless of surrounding quotes
  --   (encodes non-alpha-numeric text with unicode number).
  escapeAttribute :: a -> a
  -- | Escaping suitable for a JavaScript string (escapes non-alpha-numeric
  --   characters, using slashcodes where available and safe).
  escapeJS        :: a -> a
  -- | Escaping suitable for CSS values; replaces non-alpha-numeric text
  --   with unicode points.
  escapeCSS       :: a -> a
  -- | Percent-sign encoding for URLs. /Not unicode safe!/
  escapeURL       :: a -> a

instance Escapable [Char] where
  escapeHTML      = concatMap subsHTML
  escapeAttribute = concatMap subsAtt
  escapeJS        = concatMap subsJS
  escapeCSS       = concatMap subsCSS
  escapeURL       = concatMap subsURL

instance Escapable T.Text where
  escapeHTML      = T.concatMap subsHTML
  escapeAttribute = T.concatMap subsAtt
  escapeJS        = T.concatMap subsJS
  escapeCSS       = T.concatMap subsCSS
  escapeURL       = T.concatMap subsURL

subsHTML :: IsString a => Char -> a
subsHTML '&'  = "&amp;"
subsHTML '<'  = "&lt;"
subsHTML '>'  = "&gt;"
subsHTML '"'  = "&quot;"
subsHTML '\'' = "&#x27;"
subsHTML '/'  = "&#x2F;"
subsHTML  c   = fromString [c]

subsAtt :: IsString a => Char -> a
subsAtt c
  | isAlphaNum c = fromString [c]
  | otherwise    = fromString $ "&#x"++(toHexS c)

subsURL :: IsString a => Char -> a
subsURL c
  | isAsciiAlphaNum c = fromString [c]
  | otherwise         = fromString $ '%':toHex2 c

-- Using backslash codes only for values with no significance to HTML.
subsJS :: IsString a => Char -> a
subsJS '\\' = "\\\\"
subsJS '\r' = "\\r"
subsJS '\n' = "\\n"
subsJS '\b' = "\\b"
subsJS '\t' = "\\t"
subsJS '\f' = "\\f"
subsJS ' '  = " "
subsJS c
  | isAlphaNum c = fromString [c]
  | otherwise    = fromString $ "\\u"++toHex4 c

subsCSS :: IsString a => Char -> a
subsCSS c
  | isAlphaNum c = fromString [c]
  | otherwise    = fromString $ ('\\':toHex2or6 c)++" "

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = case fromEnum c of
  n | n >= 48 && n <= 57  -> True --Numbers
  n | n >= 65 && n <= 90  -> True --Uppercase
  n | n >= 97 && n <= 122 -> True --Lowercase
  _                       -> False

-- Variable length semicolon-terminated encoding; for HTML encoding.
toHexS :: Char -> String
toHexS c = showHex (fromEnum c) ";"

--Truncated to two bytes, for URLs.
toHex2 :: Char -> String
toHex2 c = [hexValues ! (n1 `rem` 16), hexValues ! (n2 `rem` 16)]
  where n2  = fromEnum c
        n1 = n2 `quot` 16

toHex4 :: Char -> String
toHex4 c = [hexValues ! (n1 `rem` 16), hexValues ! (n2 `rem` 16),
            hexValues ! (n3 `rem` 16), hexValues ! (n4 `rem` 16)]
  where n4 = fromEnum c
        n3 = n4 `quot` 16
        n2 = n3 `quot` 16
        n1 = n2 `quot` 16

toHex2or6 :: Char -> String
toHex2or6 c = case fromEnum c of
  n | n < 256 -> toHex2 c
  n6          -> [hexValues ! (n1 `rem` 16), hexValues ! (n2 `rem` 16),
                  hexValues ! (n3 `rem` 16), hexValues ! (n4 `rem` 16),
                  hexValues ! (n5 `rem` 16), hexValues ! (n6 `rem` 16)]
    where n5 = n6 `quot` 16
          n4 = n5 `quot` 16
          n3 = n4 `quot` 16
          n2 = n3 `quot` 16
          n1 = n2 `quot` 16

hexValues :: UArray Int Char
hexValues = listArray (0,15) ['0', '1', '2', '3', '4', '5', '6', '7', '8',
                              '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
