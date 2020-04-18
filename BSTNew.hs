module BSTNew where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

drawGenericSubTree :: (a -> [String]) -> [a] -> [String]
drawGenericSubTree drawTree = go
  where
    go [] = []
    go [t] = "|" : shift "`- " "   " (drawTree t)
    go (t:ts) = "|" : shift "+ " "|  " (drawTree t) ++ go ts

    shift first other = zipWith (++) (first : repeat other)


parseInput :: Parser (BSTNew String)
parseInput = do
  whiteSpace lexerBSTNew
  e <- parseBSTNew
  eof
  return e

convert :: String -> String
convert s =
  case ret of
    Left e -> "error: " ++ (show e)
    Right v -> "answer: \n" ++ (show v)
  where
    ret = parse parseInput "" s

main :: IO ()
main = interact (unlines . (map convert) . lines)

data BSTNewTerm a = BSTNewVar a
                  | BSTNewEmpty
                  | BSTNewPowerSet (BSTNewTerm a)
                  | BSTNewBinUnion (BSTNewTerm a) (BSTNewTerm a)

data BSTNewAtomic a = BSTNewEq (BSTNewTerm a) (BSTNewTerm a)
                   | BSTNewIn (BSTNewTerm a) (BSTNewTerm a)
                   | BSTNewIsOrdinal (BSTNewTerm a)

data BSTNew a = BSTNewAtom (BSTNewAtomic a)
              | BSTNewAnd (BSTNew a) (BSTNew a)
              | BSTNewOr (BSTNew a) (BSTNew a)
              | BSTNewImplies (BSTNew a) (BSTNew a)
              | BSTNewIff (BSTNew a) (BSTNew a)
              | BSTNewNot (BSTNew a)
              | BSTNewForAll a (BSTNew a)
              | BSTNewExists a (BSTNew a)

drawBSTNewTerm :: (Show a) => BSTNewTerm a -> [String]
drawBSTNewTerm = go
  where
    go (BSTNewVar x) = [show x]
    go (BSTNewEmpty) = ["Empty"]
    go (BSTNewPowerSet x1) = ["PowerSet"] ++ drawGenericSubTree drawBSTNewTerm [x1]
    go (BSTNewBinUnion x1 x2) = ["BinUnion"] ++ drawGenericSubTree drawBSTNewTerm [x1,x2]


drawBSTNewAtomic :: (Show a) => BSTNewAtomic a -> [String]
drawBSTNewAtomic = go
  where
    go (BSTNewEq x1 x2) = ["Eq"] ++ drawGenericSubTree drawBSTNewTerm [x1,x2]
    go (BSTNewIn x1 x2) = ["In"] ++ drawGenericSubTree drawBSTNewTerm [x1,x2]
    go (BSTNewIsOrdinal x1) = ["IsOrdinal"] ++ drawGenericSubTree drawBSTNewTerm [x1]

drawBSTNew :: (Show a) => BSTNew a -> [String]
drawBSTNew = go
  where
    go (BSTNewAtom x1) = drawBSTNewAtomic x1
    go (BSTNewAnd x1 x2) = ["And"] ++ drawGenericSubTree drawBSTNew [x1,x2]
    go (BSTNewOr x1 x2) = ["Or"] ++ drawGenericSubTree drawBSTNew [x1,x2]
    go (BSTNewImplies x1 x2) = ["Implies"] ++ drawGenericSubTree drawBSTNew [x1,x2]
    go (BSTNewIff x1 x2) = ["Iff"] ++ drawGenericSubTree drawBSTNew [x1,x2]
    go (BSTNewNot x1) = ["Not"] ++ drawGenericSubTree drawBSTNew [x1]
    go (BSTNewForAll x1 x2) = ["For all " ++ show x1] ++ drawGenericSubTree drawBSTNew [x2]
    go (BSTNewExists x1 x2) = ["Exists " ++ show x1] ++ drawGenericSubTree drawBSTNew [x2]

instance (Show a) => Show (BSTNewTerm a) where
  show = unlines . drawBSTNewTerm

instance (Show a) => Show (BSTNewAtomic a) where
  show = unlines . drawBSTNewAtomic

instance (Show a) => Show (BSTNew a) where
  show = unlines . drawBSTNew

lexerBSTNew :: TokenParser ()
lexerBSTNew = makeTokenParser languageDef
  where
    languageDef =
      emptyDef { commentStart = "/*"
               , commentEnd = "*/"
               , commentLine = "//"
               , identStart = letter
               , identLetter = alphaNum
               , reservedNames = [ "Eq"
                                 , "Not"
                                 , "ForAll"
                                 , "Exists"
                                 , "Empty"
                                 , "In"
                                 , "IsOrdinal"
                                 , "PowerSet"
                                 , "BinUnion"
                                 ]
               , reservedOpNames = [ "&&", "||", "->", "<->" ]
               }

parseBSTNewTerm :: Parser (BSTNewTerm String)
parseBSTNewTerm = parseBSTNewVar
    <|> parseBSTNewEmpty
    <|> parseBSTNewPowerSet
    <|> parseBSTNewBinUnion
  where
    parseBSTNewVar = do
      v <- identifier lexerBSTNew
      return $ BSTNewVar v

    parseBSTNewEmpty = do
      reserved lexerBSTNew "Empty"
      return BSTNewEmpty

    parseBSTNewPowerSet = do
      reserved lexerBSTNew "PowerSet"
      char '('
      whiteSpace lexerBSTNew
      x1 <- parseBSTNewTerm
      char ')'
      whiteSpace lexerBSTNew
      return $ BSTNewPowerSet x1

    parseBSTNewBinUnion = do
      reserved lexerBSTNew "BinUnion"
      char '('
      whiteSpace lexerBSTNew
      x1 <- parseBSTNewTerm
      char ','
      whiteSpace lexerBSTNew
      x2 <- parseBSTNewTerm
      char ')'
      whiteSpace lexerBSTNew
      return $ BSTNewBinUnion x1 x2


parseBSTNewAtomic :: Parser (BSTNewAtomic String)
parseBSTNewAtomic =
    parseBSTNewEq
    <|> parseBSTNewIn
    <|> parseBSTNewIsOrdinal
  where
    parseBSTNewEq = do
      reserved lexerBSTNew "Eq"
      char '('
      whiteSpace lexerBSTNew
      x1 <- parseBSTNewTerm
      char ','
      whiteSpace lexerBSTNew
      x2 <- parseBSTNewTerm
      char ')'
      whiteSpace lexerBSTNew
      return $ BSTNewEq x1 x2

    parseBSTNewIn = do
      reserved lexerBSTNew "In"
      char '('
      whiteSpace lexerBSTNew
      x1 <- parseBSTNewTerm
      char ','
      whiteSpace lexerBSTNew
      x2 <- parseBSTNewTerm
      char ')'
      whiteSpace lexerBSTNew
      return $ BSTNewIn x1 x2

    parseBSTNewIsOrdinal = do
      reserved lexerBSTNew "IsOrdinal"
      char '('
      whiteSpace lexerBSTNew
      x1 <- parseBSTNewTerm
      char ')'
      whiteSpace lexerBSTNew
      return $ BSTNewIsOrdinal x1


parseBSTNew :: Parser (BSTNew String)
parseBSTNew = (flip buildExpressionParser) parseBSTNew' $ [
    [ Prefix (reserved lexerBSTNew "Not" >> return BSTNewNot) ]
  , [ Infix (reservedOp lexerBSTNew "&&" >> return BSTNewAnd) AssocLeft ]
  , [ Infix (reservedOp lexerBSTNew "||" >> return BSTNewOr) AssocLeft ]
  , [ Infix (reservedOp lexerBSTNew "->" >> return BSTNewImplies) AssocRight
    , Infix (reservedOp lexerBSTNew "<->" >> return BSTNewIff) AssocLeft
    ]
  ]

parseBSTNew' :: Parser (BSTNew String)
parseBSTNew' = (parens lexerBSTNew parseBSTNew) <|> parseForAll <|> parseExists <|> parseAtom
  where
    parseAtom = do
      x <- parseBSTNewAtomic
      return $ BSTNewAtom x

    parseForAll = do
      reserved lexerBSTNew "ForAll"
      x <- identifier lexerBSTNew
      e <- parens lexerBSTNew parseBSTNew
      return $ BSTNewForAll x e
    parseExists = do
      reserved lexerBSTNew "Exists"
      x <- identifier lexerBSTNew
      e <- parens lexerBSTNew parseBSTNew
      return $ BSTNewExists x e

