{- ============================================================================
   MODULE: PostScript.Lexer
   PATH:   src/PostScript/Lexer.hs

   DESCRIPTION:
     Tokenizer and block structural parser built with Megaparsec (or Attoparsec).
     Converts raw PostScript string input into a flat list of executable `[Object]` items,
     handling comments, whitespace, numbers, literal names, and nested `{ ... }` blocks.

   RESPONSIBILITIES:
     1. Lexical Analysis & Whitespace Handling:
        - Strip PostScript comments (lines starting with `%`).
        - Treat all standard space, tab, newline, and carriage return characters as delimiters.
     2. Parse Literals and Numbers:
        - Parse integers (e.g. `42`, `-10`).
        - Parse floating-point numbers (e.g. `3.14159`, `-.05`).
        - Parse literal names prefixed with slash `/` (e.g. `/x`, `/myProc` -> `PSName "x"`).
        - Parse executable symbols without slash (e.g. `add`, `moveto`, `dup` -> `PSSymbol "add"`).
        - Parse string literals inside parentheses `(hello world)` -> `PSString`.
     3. Parse Deferred Code Blocks:
        - Recursively parse executable blocks enclosed in curly braces `{ ... }` into `PSBlock [Object]`.
        - Ensure proper nesting validation for curly braces.

   FUNCTIONS TO IMPLEMENT:
     - parsePostScript :: String -> Either String [Object]
         Top-level parser runner over source text.
     - sc :: Parser ()
         Space consumer (handles spaces, newlines, and comments starting with `%`).
     - pObject :: Parser Object
         Main object dispatcher trying integers, reals, names, symbols, strings, and blocks.
     - pInteger :: Parser Object
     - pReal :: Parser Object
     - pName :: Parser Object (starts with `/`)
     - pSymbol :: Parser Object (identifiers without `/`)
     - pString :: Parser Object (parenthesized strings)
     - pBlock :: Parser Object (recursive `{ ... }` parsing)
   ============================================================================ -}

module PostScript.Lexer (parsePostScript) where

import Data.Void (Void)
import PostScript.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- Space consumer: skips spaces, tabs, newlines, and '%' line comments
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "%")
    empty

-- Lexeme wrapper: consumes trailing whitespace after parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbols: match a specific string and consume trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol sc

-- Reserved delimiter characters that cannot be part of standard identifiers
isDelimiter :: Char -> Bool
isDelimiter c = c `elem` (" \t\r\n%()<>[]{}/")

-- Parse an integer or floating-point real number
pNumber :: Parser Object
pNumber = lexeme $ do
  sign <- optional (char '+' <|> char '-')
  let signMult = case sign of
        Just '-' -> -1.0
        _ -> 1.0
  digits <- some digitChar
  mDec <- optional (char '.' *> some digitChar)
  case mDec of
    Just frac -> do
      let str = digits ++ "." ++ frac
      return (PSReal (signMult * read str))
    Nothing -> do
      let intVal = read digits
      return (PSInteger (round signMult * intVal))

-- Parse literal names prefixed by '/' (e.g., /x, /myProc)
pName :: Parser Object
pName = lexeme $ do
  _ <- char '/'
  name <- some (satisfy (not . isDelimiter))
  return (PSName name)

-- Parse string literals enclosed in parentheses
pString :: Parser Object
pString = lexeme $ do
  _ <- char '('
  str <- manyTill L.charLiteral (char ')')
  return (PSString str)

-- Parse deferred executable code blocks enclosed in curly braces
pBlock :: Parser Object
pBlock = do
  _ <- symbol "{"
  objs <- many pObject
  _ <- symbol "}"
  return (PSBlock objs)

-- Parse executable symbol / operator (e.g., add, def, moveto, true, false)
pSymbol :: Parser Object
pSymbol = lexeme $ do
  ident <- some (satisfy (not . isDelimiter))
  case ident of
    "true" -> return (PSBoolean True)
    "false" -> return (PSBoolean False)
    _ -> return (PSSymbol ident)

-- Dispatcher for any PostScript object
pObject :: Parser Object
pObject =
  choice
    [ pBlock,
      pName,
      pString,
      try pNumber,
      pSymbol
    ]

-- Top-level parser for full script
pProgram :: Parser [Object]
pProgram = sc *> many pObject <* eof

-- High-level API to parse a raw PostScript string
parsePostScript :: String -> Either String [Object]
parsePostScript input =
  case runParser pProgram "" input of
    Left bundle -> Left (errorBundlePretty bundle)
    Right objs -> Right objs