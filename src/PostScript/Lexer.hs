module PostScript.Lexer (parsePostScript) where

import Data.Void (Void)
import PostScript.Environment (isBuiltin)
import PostScript.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- Space consumer: skips spaces, tabs, newlines, null bytes, and '%' line comments
sc :: Parser ()
sc =
  L.space
    (takeWhile1P (Just "whitespace") isPSWhitespace *> pure ())
    (L.skipLineComment "%")
    empty
  where
    isPSWhitespace c = c <= ' '

-- Lexeme wrapper: consumes trailing whitespace after parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbols: match a specific string and consume trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol sc

-- Reserved delimiter characters that cannot be part of standard identifiers
isDelimiter :: Char -> Bool
isDelimiter c = c <= ' ' || c `elem` "%()<>[]{}/"

-- Parse an integer or floating-point real number (including scientific notation)
pNumber :: Parser Object
pNumber = lexeme $ do
  sign <- optional (char '+' <|> char '-')
  let signMult = case sign of
        Just '-' -> -1.0
        _ -> 1.0
  let pExp = (char 'e' <|> char 'E') *> ((:) <$> (char '+' <|> char '-' <|> digitChar) <*> many digitChar)
  leadingDigits <- optional (some digitChar)
  case leadingDigits of
    Just digits -> do
      mDec <- optional (char '.' *> many digitChar)
      mExp <- optional pExp
      case (mDec, mExp) of
        (Nothing, Nothing) -> return (PSInteger (round signMult * read digits))
        _ -> do
          let fracStr = case mDec of
                Just frac | not (null frac) -> frac
                _ -> "0"
              baseStr = digits ++ "." ++ fracStr
              expStr = case mExp of
                Just (c : rest) | c `elem` ['+', '-'] -> "e" ++ (c : rest)
                Just ex -> "e+" ++ ex
                Nothing -> ""
          return (PSReal (signMult * read (baseStr ++ expStr)))
    Nothing -> do
      _ <- char '.'
      frac <- some digitChar
      mExp <- optional pExp
      let expStr = case mExp of
            Just (c : rest) | c `elem` ['+', '-'] -> "e" ++ (c : rest)
            Just ex -> "e+" ++ ex
            Nothing -> ""
      return (PSReal (signMult * read ("0." ++ frac ++ expStr)))

-- Parse immediately evaluated names prefixed by '//' (e.g., //moveto)
pImmName :: Parser Object
pImmName = lexeme $ do
  _ <- try (string "//")
  name <- some (satisfy (not . isDelimiter))
  return (if isBuiltin name then PSOperator name else PSSymbol name)

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

-- Parse bracket and angle-bracket container operators: [, ], <<, >>
pDelimOp :: Parser Object
pDelimOp =
  choice
    [ try (symbol "<<") >> return (PSSymbol "<<"),
      try (symbol ">>") >> return (PSSymbol ">>"),
      symbol "[" >> return (PSSymbol "["),
      symbol "]" >> return (PSSymbol "]")
    ]

-- Parse executable symbol / operator (e.g., add, def, moveto, true, false, null)
pSymbol :: Parser Object
pSymbol = lexeme $ do
  ident <- some (satisfy (not . isDelimiter))
  case ident of
    "true" -> return (PSBoolean True)
    "false" -> return (PSBoolean False)
    "null" -> return PSNull
    _ -> return (PSSymbol ident)

-- Dispatcher for any PostScript object
pObject :: Parser Object
pObject =
  choice
    [ pBlock,
      pDelimOp,
      pImmName,
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