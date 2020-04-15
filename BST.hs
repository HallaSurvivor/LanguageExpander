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

instance (Show a) => Show (BST a) where
  show = unlines . drawBST

-- {{{ BST Parser

parseBSTVar :: Parser (BSTTerm String)
parseBSTVar = do
  v <- many1 letter
  return $ BSTVar v

parseBSTEq :: Parser (BST String)
parseBSTEq = undefined
  

parseBST :: Parser (BST String)
parseBST = undefined


-- }}}

-- TODO: AST -> unparsed symbols

--}}}


-- {{{ Outer parser stuff -- main program

parseDesiredConversion :: Parser String
parseDesiredConversion = do
  dc <- string "convertBSTtoBST"
  return dc

conversion :: Parser String
conversion = do
  parseDesiredConversion

convert :: String -> String
convert s =
  case ret of
    Left e -> "error: " ++ (show e)
    Right o -> "converted: " ++ o
  where
    ret = parse conversion "" s

main :: IO ()
main = interact (unlines . (map convert) . lines)

-- }}}
