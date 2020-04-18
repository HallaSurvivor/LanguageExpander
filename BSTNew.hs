data BST a = BSTVar a
           | BSTEq (BST a) (BST a)
           | BSTAnd (BST a) (BST a)
           | BSTOr (BST a) (BST a)
           | BSTImplies (BST a) (BST a)
           | BSTIff (BST a) (BST a)
           | BSTNot (BST a)
           | BSTForAll a (BST a)
           | BSTExists a (BST a)
           | BSTEmpty
           | BSTIn (BST a) (BST a)
           | BSTIsOrdinal (BST a)
           | BSTPowerSet (BST a)
           | BSTBinaryUnion (BST a) (BST a)


drawBST :: (Show a) => BST a -> [String]
drawBST = go
  where
    go (BSTVar x) = [show x]
    go (BSTEq x1 x2) = ["Eq"] ++ drawSubTrees [x1,x2]
    go (BSTAnd x1 x2) = ["And"] ++ drawSubTrees [x1,x2]
    go (BSTOr x1 x2) = ["Or"] ++ drawSubTrees [x1,x2]
    go (BSTImplies x1 x2) = ["Implies"] ++ drawSubTrees [x1,x2]
    go (BSTIff x1 x2) = ["Iff"] ++ drawSubTrees [x1,x2]
    go (BSTNot x1) = ["Not"] ++ drawSubTrees [x1]
    go (BSTForAll x1 x2) = ["For all " ++ show x1] ++ drawSubTrees [x2]
    go (BSTExists x1 x2) = ["For all " ++ show x1] ++ drawSubTrees [x2]
    go (BSTEmpty) = ["Empty"]
    go (BSTIn x1 x2) = ["In"] ++ drawSubTrees [x1,x2]
    go (BSTIsOrdinal x1) = ["IsOrdinal"] ++ drawSubTrees [x1]
    go (BSTPowerSet x1) = ["PowerSet"] ++ drawSubTrees [x1]
    go (BSTBinaryUnion x1 x2) = ["BinaryUnion"] ++ drawSubTrees [x1,x2]

    drawSubTrees []     = []
    drawSubTrees [t]    = "|" : shift "`- " "   " (drawBST t)
    drawSubTrees (t:ts) = "|" : shift "+- " "|  " (drawBST t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

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
               , reservedNames = [ "Eq"
                                 , "Not"
                                 , "ForAll"
                                 , "Exists"
                                 , "Empty"
                                 , "In"
                                 , "IsOrdinal"
                                 , "PowerSet"
                                 , "BinaryUnion"
                                 ]
               , reservedOpNames = [ "&&", "||", "->", "<->" ]
               }
parseBSTTerm :: Parser (BSTTerm String)
parseBSTTerm = parseBSTVar
    <|> parseBSTEmpty
    <|> parseBSTPowerSet
    <|> parseBSTBinaryUnion
  where
    parseBSTVar = do
      v <- indentifier lexerBST
      return $ BSTVar v

    parseBSTEmpty = do
      reserved lexerBST "Empty"
      return BSTEmpty

    parseBSTPowerSet do
      reserved lexerBST "PowerSet"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTPowerSet x1

    parseBSTBinaryUnion do
      reserved lexerBST "BinaryUnion"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ','
      whiteSpace lexerBST
      x2 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTBinaryUnion x1 x2

parseBSTAtomic :: Parser (BSTAtomic String)
parseBSTAtomic =
    parseBSTEq
    <|> parseBSTIn
    <|> parseBSTIsOrdinal
  where
    parseBSTIn = do
      reserved lexerBST "In"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ','
      whiteSpace lexerBST
      x2 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTIn x1 x2

    parseBSTIsOrdinal = do
      reserved lexerBST "IsOrdinal"
      char '('
      whiteSpace lexerBST
      x1 <- parseBSTTerm
      char ')'
      whiteSpace lexerBST
      return $ BSTIsOrdinal x1

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
parseBST' = (parenslexerBST parseBST) <|> parseForAll <|> parseExists <|> parseAtom
  where
    parseAtom = do
      x <- parseBSTAtomic
      return $ BSTAtom x

    parseForAll = do
      reserved lexerBST "ForAll"
      x <- identifier lexerBST
      e <- parens lexerBST parseBST
      return $ BSTExists x e
