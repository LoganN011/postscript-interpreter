module PostScript.Lexer
  ( parsePostScript,
    parseOneToken,
  )
where

import Data.Char (isDigit, ord)
import Data.Void (Void)
import Numeric qualified
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
      mRadix <- optional (char '#' *> some alphaNumChar)
      case mRadix of
        Just rDigits -> do
          let base = read digits :: Int
              valDigit c
                | isDigit c = ord c - ord '0'
                | c >= 'a' && c <= 'z' = ord c - ord 'a' + 10
                | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 10
                | otherwise = 999
              isValidDigit c = valDigit c < base
          case Numeric.readInt (fromIntegral base) isValidDigit valDigit rDigits of
            [(val, "")] -> return (PSInteger (round signMult * val))
            _ -> fail "invalid radix number"
        Nothing -> do
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

-- Parse string literals enclosed in parentheses with balanced nested parens
pString :: Parser Object
pString = lexeme $ do
  _ <- char '('
  str <- pStringContent
  return (PSString str)

pStringContent :: Parser String
pStringContent = concat <$> manyTill pChunk (char ')')
  where
    pChunk =
      choice
        [ try ((\inner -> "(" ++ inner ++ ")") <$> (char '(' *> pStringContent)),
          (: [])
            <$> choice
              [ char '\\'
                  *> choice
                    [ char 'n' *> pure '\n',
                      char 'r' *> pure '\r',
                      char 't' *> pure '\t',
                      char 'b' *> pure '\b',
                      char 'f' *> pure '\f',
                      char '\\' *> pure '\\',
                      char '(' *> pure '(',
                      char ')' *> pure ')',
                      anySingle
                    ],
                noneOf "()"
              ]
        ]

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

-- Parses a single token from the front of an input string
parseOneToken :: String -> Maybe (Object, String)
parseOneToken input =
  case runParser ((,) <$> (sc *> pObject) <*> getInput) "" input of
    Right (obj, rest) -> Just (obj, rest)
    Left _ -> Nothing