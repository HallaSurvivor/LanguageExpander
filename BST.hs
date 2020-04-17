module BST where

import Text.Parsec
import Text.Parsec.String 
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr

-- {{{ Stuff every ST can use

drawGenericSubTree :: (a -> [String]) -> [a] -> [String]
drawGenericSubTree drawTree = go
  where
    go []     = []
    go [t]    = "|" : shift "`- " "   " (drawTree t)
    go (t:ts) = "|" : shift "+- " "|  " (drawTree t) ++ go ts

    shift first other = zipWith (++) (first : repeat other)


-- }}}

--{{{ BST stuff, do this for each extension

data BSTTerm a = BSTVar a 
               | BSTEmptySet
               | BSTPowerSet (BSTTerm a)

data BSTAtomic a = BSTEq (BSTTerm a) (BSTTerm a)
                 | BSTMembership (BSTTerm a) (BSTTerm a)

data BST a = BSTAtom (BSTAtomic a)
           | BSTAnd (BST a) (BST a)
           | BSTOr (BST a) (BST a)
           | BSTImplies (BST a) (BST a)
           | BSTIff (BST a) (BST a)
           | BSTNot (BST a)
           | BSTForAll a (BST a)
           | BSTExists a (BST a)

drawBSTTerm :: (Show a) => BSTTerm a -> [String]
drawBSTTerm = go
  where
    go (BSTVar x) = [show x]
    go (BSTEmptySet) = ["EmptySet"]
    go (BSTPowerSet x1) = ["PowerSet"] ++ drawGenericSubTree drawBSTTerm [x1]

drawBSTAtomic :: (Show a) => BSTAtomic a -> [String]
drawBSTAtomic = go
  where
    go (BSTEq x1 x2) = ["Eq"] ++ drawGenericSubTree drawBSTTerm [x1,x2]
    go (BSTMembership x1 x2) = ["Membership"] ++ drawGenericSubTree drawBSTTerm [x1,x2]

drawBST :: (Show a) => BST a -> [String]
drawBST = go
  where
    go (BSTAtom x1) = drawBSTAtomic x1
    go (BSTAnd x1 x2) = ["And"] ++ drawSubTrees [x1,x2]
    go (BSTOr x1 x2) = ["Or"] ++ drawSubTrees [x1,x2]
    go (BSTImplies x1 x2) = ["Implies"] ++ drawSubTrees [x1,x2]
    go (BSTIff x1 x2) = ["Iff"] ++ drawSubTrees [x1,x2]
    go (BSTNot x1) = ["Not"] ++ drawSubTrees [x1]
    go (BSTForAll x1 x2) = ["For all " ++ show x1] ++ drawSubTrees [x2]
    go (BSTExists x1 x2) = ["Exists " ++ show x1] ++ drawSubTrees [x2]

    drawSubTrees = drawGenericSubTree drawBST


instance (Show a) => Show (BSTTerm a) where
  show = unlines . drawBSTTerm

instance (Show a) => Show (BSTAtomic a) where
  show = unlines . drawBSTAtomic

instance (Show a) => Show (BST a) where
  show = unlines . drawBST


lexerBST :: TokenParser ()
lexerBST = makeTokenParser languageDef
  where
    languageDef =
      emptyDef { commentStart = "/*"
               , commentEnd = "*/"
               , commentLine = "//"
               , identStart = letter
               , identLetter = alphaNum
               , reservedNames = [ "Empty"
                                 , "P" 
                                 , "In"
                                 , "Eq"
                                 , "Not"
                                 , "ForAll"
                                 , "Exists"
                                 ]
               , reservedOpNames = [ "&&", "||", "->", "<->" ]
               }

parseBSTTerm :: Parser (BSTTerm String)
parseBSTTerm = parseBSTVar 
            <|> parseBSTEmptySet
            <|> parseBSTPowerSet
  where
    parseBSTVar = do 
      v <- identifier lexerBST
      return $ BSTVar v

    parseBSTEmptySet = do
      reserved lexerBST "Empty"
      return BSTEmptySet

    parseBSTPowerSet = do
      reserved lexerBST "P"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ')'
      return $ BSTPowerSet x1

parseBSTAtomic :: Parser (BSTAtomic String)
parseBSTAtomic = parseBSTEq -- do we actually need to repeat these?
              <|> parseBSTMembership
              <|> parens lexerBST parseBSTEq
              <|> parens lexerBST parseBSTMembership
  where
    parseBSTEq = do
      reserved lexerBST "Eq"
      char '('
      whiteSpace lexerBST 
      x1 <- parseBSTTerm
      char ','
      whiteSpace lexerBST
      x2 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTEq x1 x2

    parseBSTMembership = do
      reserved lexerBST "In"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ','
      whiteSpace lexerBST
      x2 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTMembership x1 x2

parseBST :: Parser (BST String)
parseBST = (flip buildExpressionParser) parseBST' $ [
    [ Prefix (reserved lexerBST "Not" >> return BSTNot) ]
  , [ Infix (reservedOp lexerBST "&&" >> return BSTAnd) AssocLeft ]
  , [ Infix (reservedOp lexerBST "||" >> return BSTOr) AssocLeft ]
  , [ Infix (reservedOp lexerBST "->" >> return BSTImplies) AssocRight
    , Infix (reservedOp lexerBST "<->" >> return BSTIff) AssocLeft
    ]
  ]

parseBST' :: Parser (BST String)
parseBST' = (parens lexerBST parseBST) <|> parseBSTForAll <|> parseBSTExists <|> parseBSTAtom 
  where
    parseBSTAtom = do
      x <- parseBSTAtomic
      return $ BSTAtom x

    parseBSTForAll = do
      reserved lexerBST "ForAll"
      x <- identifier lexerBST
      e <- parens lexerBST parseBST
      return $ BSTForAll x e

    parseBSTExists = do
      reserved lexerBST "Exists"
      x <- identifier lexerBST
      e <- parens lexerBST parseBST
      return $ BSTExists x e


-- TODO: AST -> unparsed symbols

--}}}


-- {{{ Outer parser stuff -- main program

parseInput :: Parser (BST String)
parseInput = do
  whiteSpace lexerBST
  e <- parseBST
  eof
  return e

convert :: String -> String
convert s =
  case ret of
    Left  e -> "error: " ++ (show e)
    Right v -> "answer: \n" ++ (show v)
  where
    ret = parse parseInput "" s

main :: IO ()
main = interact (unlines . (map convert) . lines)

-- }}}
