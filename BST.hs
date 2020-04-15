module BST where

import Text.Parsec
import Text.Parsec.String 

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
    go (BSTExists x1 x2) = ["For all " ++ show x1] ++ drawSubTrees [x2]

    drawSubTrees = drawGenericSubTree drawBST


instance (Show a) => Show (BSTTerm a) where
  show = unlines . drawBSTTerm

instance (Show a) => Show (BSTAtomic a) where
  show = unlines . drawBSTAtomic

instance (Show a) => Show (BST a) where
  show = unlines . drawBST


parseBSTVar :: Parser (BSTTerm String)
parseBSTVar = do
  v <- many1 letter
  return $ BSTVar v

parseBSTEmptySet :: Parser (BSTTerm String)
parseBSTEmptySet = do
  string "EmptySet"
  return $ BSTEmptySet

parseBSTPowerSet :: Parser (BSTTerm String)
parseBSTPowerSet = do
  string "P"
  char '('
  x1 <- parseBSTTerm
  char ')'
  return $ BSTPowerSet x1
  
parseBSTTerm :: Parser (BSTTerm String)
parseBSTTerm = do
  try parseBSTPowerSet <|> try parseBSTEmptySet <|> parseBSTVar


parseBSTEq :: Parser (BSTAtomic String)
parseBSTEq = do
  char '('
  x1 <- parseBSTTerm
  char ')'
  char '='
  char '('
  x2 <- parseBSTTerm
  char ')'
  return $ BSTEq x1 x2

parseBSTMembership :: Parser (BSTAtomic String)
parseBSTMembership = do
  string "Member"
  char '('
  x1 <- parseBSTTerm
  char ','
  x2 <- parseBSTTerm
  char ')'
  return $ BSTMembership x1 x2

parseBSTAtomic :: Parser (BSTAtomic String)
parseBSTAtomic = do
  try parseBSTEq <|> try parseBSTMembership


parseBSTAtom :: Parser (BST String)
parseBSTAtom = do
  x <- parseBSTAtomic
  return $ BSTAtom x

parseBSTAnd :: Parser (BST String)
parseBSTAnd = do
  char '('
  x1 <- parseBST
  char ')'
  string "&&"
  char '('
  x2 <- parseBST
  char ')'
  return $ BSTAnd x1 x2

parseBSTOr :: Parser (BST String)
parseBSTOr = do
  char '('
  x1 <- parseBST
  char ')'
  string "||"
  char '('
  x2 <- parseBST
  char ')'
  return $ BSTOr x1 x2

parseBSTImplies :: Parser (BST String)
parseBSTImplies = do
  char '('
  x1 <- parseBST
  char ')'
  string "->"
  char '('
  x2 <- parseBST
  char ')'
  return $ BSTImplies x1 x2

parseBSTIff :: Parser (BST String)
parseBSTIff = do
  char '('
  x1 <- parseBST
  char ')'
  string "<->"
  char '('
  x2 <- parseBST
  char ')'
  return $ BSTIff x1 x2

parseBSTNot :: Parser (BST String)
parseBSTNot = do
  string "Not"
  char '('
  x1 <- parseBST
  char ')'
  return $ BSTNot x1

parseBSTForAll :: Parser (BST String)
parseBSTForAll = do
  string "[(ForAll "
  v <- many1 letter
  string ")"
  x1 <- parseBST
  string "]"
  return $ BSTForAll v x1

parseBSTExists :: Parser (BST String)
parseBSTExists = do
  string "[(Exists "
  v <- many1 letter
  string ")"
  x1 <- parseBST
  string "]"
  return $ BSTExists v x1

parseBST :: Parser (BST String)
parseBST = do
  try parseBSTAtom <|> try parseBSTAnd <|> try parseBSTOr <|> try parseBSTImplies <|> try parseBSTIff <|> try parseBSTNot <|> try parseBSTForAll <|> parseBSTExists


-- TODO: AST -> unparsed symbols

--}}}


-- {{{ Outer parser stuff -- main program

convert :: String -> String
convert s =
  case ret of
    Left  e -> "error: " ++ (show e)
    Right v -> "answer: \n" ++ (show v)
  where
    ret = parse parseBST "" s

main :: IO ()
main = interact (unlines . (map convert) . lines)

-- }}}
