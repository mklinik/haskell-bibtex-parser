module Text.BibTeX.Entry where

import Data.Char (toLower, )
import Data.Tuple.HT (mapFst, )

-- BibTeX has two kinds of field values, with and without quotes.
--   author = {John Doe}
--   month = jan
-- Our parser must remember which one it saw, because BibTeX treats naked
-- values special.
data FieldValue
  = Naked String
  | Quoted String
  deriving Show

-- Bibtex supports four kinds of entries:
--  @string, @preamble, @comment and bibliography items
-- Only @string and bibliography items are supported for now.
-- http://www.bibtex.org/Format/
data T
  = Entry
    { entryType :: String
    , identifier :: String
    -- field values in general are of the form "foo" # bar # "baz"
    -- where # denotes string concatenation and bar is an identifier referring to a pre-defined string
    , fields :: [(String, [FieldValue])]
    }
   | BibString String [FieldValue]
   | Comment String
   deriving (Show)

{- |
Convert the name style \"Surname, First name\" into \"First name Surname\".
-}
flipName :: String -> String
flipName name =
   let (surname, firstName) = break (','==) name
   in  dropWhile (flip elem ", ") firstName ++ " " ++ surname

lowerCaseFieldNames :: T -> T
lowerCaseFieldNames t@(BibString _ _) = t
lowerCaseFieldNames t@(Comment _) = t
lowerCaseFieldNames entry =
   entry {fields = map (mapFst (map toLower)) $ fields entry}
