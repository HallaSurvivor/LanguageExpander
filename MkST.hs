-- A module to make ASTs for Main.hs
module MkST where
import System.IO
import Control.Monad.Reader

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

  let syntaxTree = makeTermData name cs rs fs ++ "\n" ++
                   makeAtomicData name cs rs fs ++ "\n" ++
                   makeData name cs rs fs ++ "\n" ++
                   makeTermPrettyPrinter name cs rs fs ++ "\n" ++
                   makeAtomicPrettyPrinter name cs rs fs ++ "\n" ++
                   makePrettyPrinter name cs rs fs ++ "\n" ++
                   makeShowDefinitions name cs rs fs ++ "\n"

  let parser = mkLexer name cs rs fs ++ "\n" ++
               mkTermParser name cs rs fs ++ "\n" ++
               mkAtomicParser name cs rs fs ++ "\n" ++
               mkParser name cs rs fs ++ "\n"

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

makeTermData :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeTermData name cs rs fs = unlines $ topLine ++ constLines ++ funLines
  where
    padding = take (length name + 12) $ repeat ' '
    prefix = padding ++ "| "

    mkArgs n = concat $ take n $ repeat $ " (" ++ name ++ "Term a)"
    mkLine (symbol, arity) = prefix ++ name ++ symbol ++ (mkArgs arity)

    topLine = ["data " ++ name ++ "Term a = " ++ name ++ "Var a"]

    constLines = fmap (\c -> prefix ++ name ++ c) cs

    funLines = fmap mkLine fs

makeAtomicData :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeAtomicData name cs rs fs = unlines $ topLine ++ relLines
  where
    padding = take (length name + 13) $ repeat ' '
    prefix = padding ++ "| "

    mkArgs n = concat $ take n $ repeat $ " (" ++ name ++ "Term a)"
    mkLine (symbol, arity) = prefix ++ name ++ symbol ++ (mkArgs arity)

    topLine = ["data " ++ name ++ "Atomic a = " ++ name ++ "Eq (" ++ name ++ "Term a) (" ++ name ++ "Term a)"]

    relLines = fmap mkLine rs

makeData :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeData name cs rs fs = unlines $ topLine ++ logicLines
  where
    padding = take (length name + 8) $ repeat ' '
    prefix = padding ++ "| "

    mkArgs n = concat $ take n $ repeat $ " (" ++ name ++ " a)"
    mkLine (symbol, arity) = prefix ++ name ++ symbol ++ (mkArgs arity)

    topLine = ["data " ++ name ++ " a = " ++ name ++ "Atom (" ++ name ++ "Atomic a)"]

    logicLines = [ mkLine ("And", 2)
                 , mkLine ("Or", 2)
                 , mkLine ("Implies", 2)
                 , mkLine ("Iff", 2)
                 , mkLine ("Not", 1)
                 , mkLine ("ForAll a", 1)
                 , mkLine ("Exists a", 1)
                 ]
  
makeTermPrettyPrinter :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeTermPrettyPrinter name cs rs fs = unlines $ topLines ++ constLines ++ funLines ++ [""]
  where
    prefix = "    go "

    mkArgs n = concat $ take n $ map (" x"++) $ map show [1..]
    mkTreeArgs n = drop 1 $ concat $ take n $ map (",x"++) $ map show [1..]

    mkLine (symbol, arity) = pattern ++ " = " ++ call
      where
        pattern = prefix ++ "(" ++ name ++ symbol ++ (mkArgs arity) ++ ")"
        call = "[\"" ++ symbol ++ "\"]" ++ " ++ drawGenericSubTree draw" ++ name ++ "Term [" ++ (mkTreeArgs arity) ++ "]"

    topLines = [ "draw" ++ name ++ "Term :: (Show a) => " ++ name ++ "Term a -> [String]" 
               , "draw" ++ name ++ "Term = go"
               , "  where"
               , "    go (" ++ name ++ "Var x) = [show x]"
               ]

    constLines = fmap (\c -> prefix ++ "(" ++ name ++ c ++ ") = [\"" ++ c ++ "\"]") cs
    funLines = fmap mkLine fs

makeAtomicPrettyPrinter :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeAtomicPrettyPrinter name cs rs fs = unlines $ topLines ++ relLines
  where
    prefix = "    go "

    mkArgs n = concat $ take n $ map (" x"++) $ map show [1..]
    mkTreeArgs n = drop 1 $ concat $ take n $ map (",x"++) $ map show [1..]

    mkLine (symbol, arity) = pattern ++ " = " ++ call
      where
        pattern = prefix ++ "(" ++ name ++ symbol ++ (mkArgs arity) ++ ")"
        call = "[\"" ++ symbol ++ "\"]" ++ " ++ drawGenericSubTree draw" ++ name ++ "Term [" ++ (mkTreeArgs arity) ++ "]"

    topLines = [ "draw" ++ name ++ "Atomic :: (Show a) => " ++ name ++ "Atomic a -> [String]"
               , "draw" ++ name ++ "Atomic = go"
               , "  where"
               ]

    relLines = fmap mkLine $ ("Eq", 2) : rs

makePrettyPrinter :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makePrettyPrinter name cs rs fs = unlines $ topLines ++ logicLines
  where
    prefix = "    go "

    mkArgs n = concat $ take n $ map (" x"++) $ map show [1..]
    mkTreeArgs n = drop 1 $ concat $ take n $ map (",x"++) $ map show [1..]

    mkLine (symbol, arity) = pattern ++ " = " ++ call
      where
        pattern = prefix ++ "(" ++ name ++ symbol ++ (mkArgs arity) ++ ")"
        call = "[\"" ++ symbol ++ "\"]" ++ " ++ drawGenericSubTree draw" ++ name ++ " [" ++ (mkTreeArgs arity) ++ "]"

    topLines = [ "draw" ++ name ++ " :: (Show a) => " ++ name ++ " a -> [String]"
               , "draw" ++ name ++ " = go"
               , "  where"
               , "    go (" ++ name ++ "Atom x1) = draw" ++ name ++ "Atomic x1"
               ]

    logicLines = [ mkLine ("And", 2)
                 , mkLine ("Or", 2)
                 , mkLine ("Implies", 2)
                 , mkLine ("Iff", 2)
                 , mkLine ("Not", 1)
                 , "    go (" ++ name ++ "ForAll x1 x2) = [\"For all \" ++ show x1] ++ drawGenericSubTree draw" ++ name ++ " [x2]"
                 , "    go (" ++ name ++ "Exists x1 x2) = [\"Exists \" ++ show x1] ++ drawGenericSubTree draw" ++ name ++ " [x2]"
                 ]
    
makeShowDefinitions :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeShowDefinitions name cs rs fs = unlines 
  [ "instance (Show a) => Show (" ++ name ++ "Term a) where"
  , "  show = unlines . draw" ++ name ++ "Term"
  , ""
  , "instance (Show a) => Show (" ++ name ++ "Atomic a) where"
  , "  show = unlines . draw" ++ name ++ "Atomic"
  , ""
  , "instance (Show a) => Show (" ++ name ++ " a) where"
  , "  show = unlines . draw" ++ name
  ]

-- }}}

-- {{{ Make the parser

mkLexer :: String -> [String] -> [(String,Int)] -> [(String,Int)] -> String
mkLexer name cs rs fs = unlines $ header ++ reservedNames ++ footer
  where
    header = [ "lexer" ++ name ++ " :: TokenParser ()"
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
             ]

    reservedNames = fmap (\n -> padding ++ n ++ "\"") ns
      where
        padding = "                                 , \""
        ns = cs ++ map fst rs ++ map fst fs

    footer = [ "                                 ]"
             , "               , reservedOpNames = [ \"&&\", \"||\", \"->\", \"<->\" ]"
             , "               }"
             ]
      

mkTermParser :: String -> [String] -> [(String,Int)] -> [(String, Int)] -> String
mkTermParser name cs rs fs = unlines $ header ++ otherCases ++ whereVar ++ constDefns ++ funDefns
  where
    header = [ "parse" ++ name ++ "Term :: Parser (" ++ name ++ "Term String)" 
             , "parse" ++ name ++ "Term = parse" ++ name ++ "Var"
             ]

    otherCases = fmap mkCase (cs ++ fmap fst fs)
      where
        mkCase f = "    <|> parse" ++ name ++ f

    whereVar = [ "  where"
               , "    parse" ++ name ++ "Var = do"
               , "      v <- identifier lexer" ++ name
               , "      return $ " ++ name ++ "Var v"
               , ""
               ]

    constDefns = concat $ fmap mkDefn cs
      where
        mkDefn c = [ "    parse" ++ name ++ c ++ " = do"
                   , "      reserved lexer" ++ name ++ " \"" ++ c ++"\""
                   , "      return " ++ name ++ c
                   , ""
                   ]

    funDefns = concat $ fmap mkDefn fs
      where
        mkLine [n] = [ "      x" ++ (show n) ++ " <- parse" ++ name ++ "Term" 
                     , "      char \')\'"
                     , "      whiteSpace lexer" ++ name
                     ]
        mkLine (i:is) =  [ "      x" ++ (show i) ++ " <- parse" ++ name ++ "Term"
                         , "      char \',\'"
                         , "      whiteSpace lexer" ++ name
                         ] ++ (mkLine is)

        vars n = concat $ fmap (\i -> " x" ++ show i) [1..n]

        mkDefn (f,n) = [ "    parse" ++ name ++ f ++ " = do" 
                       , "      reserved lexer" ++ name ++ " \"" ++ f ++ "\""
                       , "      char \'(\'"
                       , "      whiteSpace lexer" ++ name
                       ] ++ mkLine [1..n] ++ ["      return $ " ++ name ++ f ++ (vars n), ""]

mkAtomicParser :: String -> [String] -> [(String,Int)] -> [(String,Int)] -> String
mkAtomicParser name cs rs fs = unlines $ header ++ otherCases ++ ["  where"] ++ otherDefinitions
  where
    header = [ "parse" ++ name ++ "Atomic :: Parser (" ++ name ++ "Atomic String)"
             , "parse" ++ name ++ "Atomic ="  
             , "    parse" ++ name ++ "Eq"
             ]

    otherCases = fmap mkCase rs
      where
        mkCase (r,_) = "    <|> parse" ++ name ++ r

    otherDefinitions = concat $ fmap mkDefn $ ("Eq", 2) : rs
      where
        mkLine [n]    = [ "      x" ++ (show n) ++ " <- parse" ++ name ++ "Term"
                        , "      char \')\'"
                        , "      whiteSpace lexer" ++ name
                        ]
        mkLine (i:is) = [ "      x" ++ (show i) ++ " <- parse" ++ name ++ "Term"
                        , "      char \',\'"
                        , "      whiteSpace lexer" ++ name
                        ] ++ (mkLine is)

        vars n = concat $ fmap (\i -> " x" ++ show i) [1..n]

        mkDefn (r,n) = [ "    parse" ++ name ++ r ++ " = do"
                       , "      reserved lexer" ++ name ++ " \"" ++ r ++ "\""
                       , "      char \'(\'"
                       , "      whiteSpace lexer" ++ name
                       ] ++ mkLine [1..n] ++ 
                       ["      return $ " ++ name ++ r ++ (vars n), ""]


mkParser :: String -> [String] -> [(String,Int)] -> [(String,Int)] -> String
mkParser name cs rs fs = unlines wholeThing
  where
    lexName = "lexer" ++ name

    wholeThing = [ "parse" ++ name ++ " :: Parser ("++ name ++ " String)"
                 , "parse" ++ name ++ " = (flip buildExpressionParser) parse" ++ name ++ "' $ ["
                 , "    [ Prefix (reserved " ++ lexName ++ " \"Not\" >> return " ++ name ++ "Not) ]"
                 , "  , [ Infix (reservedOp " ++ lexName ++ " \"&&\" >> return " ++ name ++ "And) AssocLeft ]"
                 , "  , [ Infix (reservedOp " ++ lexName ++ " \"||\" >> return " ++ name ++ "Or) AssocLeft ]"
                 , "  , [ Infix (reservedOp " ++ lexName ++ " \"->\" >> return " ++ name ++ "Implies) AssocRight"
                 , "    , Infix (reservedOp " ++ lexName ++ " \"<->\" >> return " ++ name ++ "Iff) AssocLeft"
                 , "    ]"
                 , "  ]"
                 , ""
                 , "parse" ++ name ++ "' :: Parser (" ++ name ++ " String)"
                 , "parse" ++ name ++ "' = (parens " ++ lexName ++ " parse" ++ name ++") <|> parseForAll <|> parseExists <|> parseAtom"
                 , "  where"
                 , "    parseAtom = do"
                 , "      x <- parse" ++ name ++ "Atomic"
                 , "      return $ " ++ name ++ "Atom x"
                 , ""
                 , "    parseForAll = do"
                 , "      reserved " ++ lexName ++ " \"ForAll\""
                 , "      x <- identifier " ++ lexName
                 , "      e <- parens " ++ lexName ++ " parse" ++ name
                 , "      return $ " ++ name ++ "ForAll x e"
                 , "    parseExists = do"
                 , "      reserved " ++ lexName ++ " \"Exists\""
                 , "      x <- identifier " ++ lexName
                 , "      e <- parens " ++ lexName ++ " parse" ++ name
                 , "      return $ " ++ name ++ "Exists x e"
                 ]

-- }}}

