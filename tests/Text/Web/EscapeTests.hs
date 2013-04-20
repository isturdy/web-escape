module Text.Web.EscapeTests (escapeTests) where

import           Data.Char
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Property

import           Data.Text                            (Text, pack, unpack)
import qualified Data.Text                            as T

import           Text.Web.Escape

escapeTests = testGroup "Text.Web.Escape" [
    consistencyTests
  , forbiddenTests
  ]

-- All escapers must be equivalent.
consistencyTests = testGroup "Consistency" [
    testProperty "HTML"      prop_com_HTML
  , testProperty "Attribute" prop_com_Attribute
  , testProperty "JS"        prop_com_JS
  , testProperty "CSS"       prop_com_CSS
  , testProperty "URL"       prop_com_URL
  ]

prop_com_HTML :: String -> Bool
prop_com_HTML s = (pack $ escapeHTML s) == (escapeHTML $ pack s)

prop_com_Attribute :: String -> Bool
prop_com_Attribute s = (pack $ escapeAttribute s) == (escapeAttribute $ pack s)

prop_com_JS :: String -> Bool
prop_com_JS s = (pack $ escapeJS s) == (escapeJS $ pack s)

prop_com_CSS :: String -> Bool
prop_com_CSS s = (pack $ escapeCSS s) == (escapeCSS $ pack s)

prop_com_URL :: String -> Bool
prop_com_URL s = (pack $ escapeURL s) == (escapeURL $ pack s)

-- Does the output include forbidden characters? If not, it should be safe,
-- although not necessarily correct.
forbiddenTests = testGroup "Forbidden" [
    testProperty "HTML"      prop_forb_HTML
  , testProperty "Attribute" prop_forb_Att
  , testProperty "JS"        prop_forb_JS
  , testProperty "CSS"       prop_forb_CSS
  , testProperty "URL"       prop_forb_URL
  ]

prop_forb_HTML :: String -> Bool
prop_forb_HTML s = rem == []
  where rem = filter (inSet "<>\"'/") $ escapeHTML s

prop_forb_Att :: String -> Bool
prop_forb_Att s = rem == []
  where rem = filter (not . inSet "&#;") . filter (not . isAlphaNum)
              $ escapeAttribute s

prop_forb_JS :: String -> Bool
prop_forb_JS s = rem == []
  where rem = filter (not . inSet "\\ ") . filter (not . isAlphaNum)
              $ escapeJS s

prop_forb_CSS :: String -> Bool
prop_forb_CSS s = rem == []
  where rem = filter (not . inSet "\\ ") . filter (not . isAlphaNum)
              $ escapeCSS s

prop_forb_URL :: String -> Bool
prop_forb_URL s = rem == []
  where rem = filter (/='%') . filter (not . isAsciiAlphaNum) $ escapeURL s

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = case fromEnum c of
  n | n >= 48 && n <= 57  -> True --Numbers
  n | n >= 65 && n <= 90  -> True --Uppercase
  n | n >= 97 && n <= 122 -> True --Lowercase
  _                       -> False

inSet :: String -> Char -> Bool
inSet s c = foldr (\tc a ->  (c == tc) || a) False s
