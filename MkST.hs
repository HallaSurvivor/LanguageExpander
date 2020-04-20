-- A module to make ASTs for Main.hs
module MkST where
import System.IO
import Control.Monad.Reader
import Text.Printf

data Theory = Theory { name :: String
                     , constants :: [String]
                     , functions :: [(String, Int)]
                     , relations :: [(String, Int)]
                     }

type Generator a = Reader Theory a

main :: IO ()
main = do
  putStrLn "Language abbreviation?"
  name <- getLine

  putStrLn "Constants? (space separated list)"
  csn <- getLine

  putStrLn "Relations? (space separated list of names)"
  rsn <- getLine

  putStrLn "and their arities? (space separated list of nats)"
  rsa <- getLine

  putStrLn "Functions? (space separated list of names)"
  fsn <- getLine

  putStrLn "and their arities? (space separated list of nats)"
  fsa <- getLine

  putStrLn "Where should I save these?"
  fname <- getLine

  let cs = words csn
      rs = zip (words rsn) (fmap read (words rsa))
      fs = zip (words fsn) (fmap read (words fsa))

  let syntaxTree = unlines [mkDataTypes name cs rs fs, mkPrettyPrinter name cs rs fs]

  let parser = unlines [mkLexer name cs rs fs, mkParsers name cs rs fs]

  writeFile fname ((boilerplate name) ++ syntaxTree ++ parser)

-- {{{ Top Level Stuff

boilerplate name = 
  unlines $ [ "module " ++ name ++ " where"
            , "import Text.Parsec"
            , "import Text.Parsec.String"
            , "import Text.Parsec.Token"
            , "import Text.Parsec.Language"
            , "import Text.Parsec.Expr"
            , ""
            , "drawGenericSubTree :: (a -> [String]) -> [a] -> [String]"
            , "drawGenericSubTree drawTree = go"
            , "  where"
            , "    go [] = []"
            , "    go [t] = \"|\" : shift \"`- \" \"   \" (drawTree t)"
            , "    go (t:ts) = \"|\" : shift \"+ \" \"|  \" (drawTree t) ++ go ts"
            , ""
            , "    shift first other = zipWith (++) (first : repeat other)"
            , ""
            , ""
            , "parseInput :: Parser (" ++ name ++ " String)"
            , "parseInput = do"
            , "  whiteSpace lexer" ++ name
            , "  e <- parse" ++ name
            , "  eof"
            , "  return e"
            , ""
            , "convert :: String -> String"
            , "convert s ="
            , "  case ret of"
            , "    Left e -> \"error: \" ++ (show e)"
            , "    Right v -> \"answer: \\n\" ++ (show v)"
            , "  where"
            , "    ret = parse parseInput \"\" s"
            , ""
            , "main :: IO ()"
            , "main = interact (unlines . (map convert) . lines)"
            , ""
            ]


-- }}}

-- {{{ Make the datatype

mkDataTypes :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
mkDataTypes name cs rs fs = unlines $ [terms, atomics, formulas]
  where
    mkLine label (symbol, arity) = "  | " ++ name ++ symbol ++ mkArgs
      where
        mkArgs = concat $ take arity $ repeat $ printf " (%s%s a)" name label

    terms = unlines $ 
      [ printf "data %sTerm a =" name
      , printf "    %sVar a" name
      ] ++ 
      (fmap (mkLine "Term") $ zip cs (repeat 0)) ++
      fmap (mkLine "Term") fs

    atomics = unlines $ 
      [ printf "data %sAtomic a =" name
      , printf "    %sEq (%sTerm a) (%sTerm a)" name name name
      ] ++ 
      fmap (mkLine "Term") rs

    formulas = unlines $
      [ printf "data %s a = %sAtom (%sAtomic a)" name name name
      , mkLine "" ("And", 2)
      , mkLine "" ("Or", 2)
      , mkLine "" ("Implies", 2)
      , mkLine "" ("Iff", 2)
      , mkLine "" ("Not", 1)
      , mkLine "" ("ForAll a", 1)
      , mkLine "" ("Exists a", 1)
      ]
  
mkPrettyPrinter :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
mkPrettyPrinter name cs rs fs = unlines $ [terms, atomics, formulas, showDefns]
  where
    mkLine (symbol, arity) = 
        printf "    go (%s%s%s) = [\"%s\"] ++ draw [%s]" name symbol args symbol treeArgs
      where
        args = concat $ take arity $ map (" x"++) $ map show [1..]
        treeArgs = drop 1 $ concat $ take arity $ map (",x"++) $ map show [1..]

    terms = unlines $
      [ printf "draw%sTerm :: (Show a) => %sTerm a -> [String]" name name
      , printf "draw%sTerm = go" name
      , "  where"
      , printf "    draw = drawGenericSubTree draw%sTerm" name
      , printf "    go (%sVar x) = [show x]" name
      ] ++
      (fmap (\c -> printf "    go (%s%s) = [\"%s\"]" name c c) cs) ++ 
      fmap mkLine fs

    atomics = unlines $
      [ printf "draw%sAtomic :: (Show a) => %sAtomic a -> [String]" name name
      , printf "draw%sAtomic = go" name
      , "  where"
      , printf "    draw = drawGenericSubTree draw%sTerm" name
      ] ++
      (fmap mkLine $ ("Eq", 2) : rs)

    formulas = unlines
      [ printf "draw%s :: (Show a) => %s a -> [String]" name name
      , printf "draw%s = go" name
      , "  where"
      , printf "    draw = drawGenericSubTree draw%s" name
      , printf "    go (%sAtom x1) = draw%sAtomic x1" name name
      , mkLine ("And", 2)
      , mkLine ("Or", 2)
      , mkLine ("Implies", 2)
      , mkLine ("Iff", 2)
      , mkLine ("Not", 1)
      , printf "    go (%sForAll x1 x2) = [\"ForAll \" ++ show x1] ++ draw [x2]" name
      , printf "    go (%sExists x1 x2) = [\"Exists \" ++ show x1] ++ draw [x2]" name
      ]

    showDefns = unlines
      [ printf "instance (Show a) => Show (%sTerm a) where" name
      , printf "  show = unlines . draw%sTerm" name
      , ""
      , printf "instance (Show a) => Show (%sAtomic a) where" name
      , printf "  show = unlines . draw%sAtomic" name
      , ""
      , printf "instance (Show a) => Show (%s a) where" name
      , printf "  show = unlines . draw%s" name
      ]

-- }}}

-- {{{ Make the parser

mkLexer :: String -> [String] -> [(String,Int)] -> [(String,Int)] -> String
mkLexer name cs rs fs = unlines $ 
  [ "lexer" ++ name ++ " :: TokenParser ()"
  , "lexer" ++ name ++ " = makeTokenParser languageDef"
  , "  where"
  , "    languageDef ="
  , "      emptyDef { commentStart = \"/*\""
  , "               , commentEnd = \"*/\""
  , "               , commentLine = \"//\""
  , "               , identStart = letter"
  , "               , identLetter = alphaNum"
  , "               , reservedNames = [ \"Eq\""
  , "                                 , \"Not\""
  , "                                 , \"ForAll\""
  , "                                 , \"Exists\""
  ] ++ 
  (
    fmap (printf "                                 , \"%s\"") $ 
      cs ++ map fst rs ++ map fst fs
  ) ++
  [ "                                 ]"
  , "               , reservedOpNames = [ \"&&\", \"||\", \"->\", \"<->\" ]"
  , "               }"
  ]

mkParsers :: String -> [String] -> [(String,Int)] -> [(String, Int)] -> String
mkParsers name cs rs fs = unlines $ [terms, atomics, formulas]
  where
    mkLine [n] = [ printf "      x%d <- parse%sTerm" n name
                 ,        "      char \')\'"
                 , printf "      whiteSpace lexer%s" name
                 ]
    mkLine (i:is) =  [ printf "      x%d <- parse%sTerm" i name
                     ,        "      char \',\'"
                     , printf "      whiteSpace lexer%s" name
                     ] ++ (mkLine is)

    vars :: Int -> String
    vars n = concat $ fmap (printf " x%d") [1..n]

    mkDefn (symbol, arity) = 
      [ printf "    parse%s%s = do" name symbol
      , printf "      reserved lexer%s \"%s\"" name symbol
      ,        "      char \'(\'"
      , printf "      whiteSpace lexer%s" name
      ] ++ (mkLine [1..arity]) ++ 
      [ printf "      return $ %s%s%s" name symbol (vars arity)
      , ""
      ]

    mkConsts symbol = 
      [ printf "    parse%s%s = do" name symbol
      , printf "      reserved lexer%s \"%s\"" name symbol
      , printf "      return %s%s" name symbol
      , ""
      ]
    
    terms = unlines $
      [ printf "parse%sTerm :: Parser (%sTerm String)" name name
      , printf "parse%sTerm = parse%sVar" name name
      ] ++
      (fmap (printf "    <|> parse%s%s" name) (cs ++ fmap fst fs)) ++
      [ "  where"
      , "    parse" ++ name ++ "Var = do"
      , "      v <- identifier lexer" ++ name
      , "      return $ " ++ name ++ "Var v"
      , ""
      ] ++
      (concat $ fmap mkConsts cs) ++ 
      (concat $ fmap mkDefn fs)

    atomics = unlines $
      [ printf "parse%sAtomic :: Parser (%sAtomic String)" name name
      , printf "parse%sAtomic =" name
      , printf "    parse%sEq" name
      ] ++ 
      (fmap (\(r,_) -> printf "    <|> parse%s%s" name r) rs) ++
      [ "  where" ] ++
      (concat $ fmap mkDefn $ ("Eq", 2):rs)

    formulas = unlines $
      [ printf "parse%s :: Parser (%s String)" name name
      , printf "parse%s = (flip buildExpressionParser) parse%s' $ [" name name
      , printf "    [ Prefix (reserved lexer%s \"Not\" >> return %sNot) ]" name name
      , printf "  , [ Infix (reservedOp lexer%s \"&&\" >> return %sAnd) AssocLeft ]" name name
      , printf "  , [ Infix (reservedOp lexer%s \"||\" >> return %sOr) AssocLeft ]" name name
      , printf "  , [ Infix (reservedOp lexer%s \"->\" >> return %sImplies) AssocRight" name name
      , printf "    , Infix (reservedOp lexer%s \"<->\" >> return %sIff) AssocLeft" name name
      ,        "    ]"
      ,        "  ]"
      , ""
      , printf "parse%s' :: Parser (%s String)" name name
      , printf "parse%s' = (parens lexer%s parse%s) <|> parseForAll <|> parseExists <|> parseAtom" name name name
      ,        "  where"
      ,        "    parseAtom = do"
      , printf "      x <- parse%sAtomic" name
      , printf "      return $ %sAtom x" name
      , ""
      ,        "    parseForAll = do"
      , printf "      reserved lexer%s \"ForAll\"" name
      , printf "      x <- identifier lexer%s" name
      , printf "      e <- parens lexer%s parse%s" name name
      , printf "      return $ %sForAll x e" name
      ,        "    parseExists = do"
      , printf "      reserved lexer%s \"Exists\"" name
      , printf "      x <- identifier lexer%s" name
      , printf "      e <- parens lexer%s parse%s" name name
      , printf "      return $ %sExists x e" name
      ]

-- }}}

