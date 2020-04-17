-- A module to make ASTs for Main.hs
module MkST (makeST) where
import System.IO

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

  writeFile fname (makeST name cs rs fs)

-- {{{ Make the datatype

makeData :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeData name cs rs fs = unlines $ topLine ++ logicLines ++ constLines ++ relLines ++ funLines ++ bottomLine
  where
    padding = take (length name + 8) $ repeat ' '
    prefix = padding ++ "| "

    mkArgs n = concat $ take n $ repeat $ " (" ++ name ++ " a)"
    mkLine (symbol, arity) = prefix ++ name ++ symbol ++ (mkArgs arity)

    topLine = ["data " ++ name ++ " a = " ++ name ++ "Var a"]

    logicLines = [ mkLine ("Eq", 2)
                , mkLine ("And", 2)
                , mkLine ("Or", 2)
                , mkLine ("Implies", 2)
                , mkLine ("Iff", 2)
                , mkLine ("Not", 1)
                , mkLine ("ForAll a", 1)
                , mkLine ("Exists a", 1)
                ]

    constLines = fmap (\c -> mkLine (c,0)) cs
    relLines   = fmap mkLine rs
    funLines   = fmap mkLine fs

    bottomLine = []
  
makePrettyPrinter :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makePrettyPrinter name cs rs fs = unlines $ concat full
  where
    prefix = "    go "

    mkArgs n = concat $ take n $ map (" x"++) $ map show [1..]
    mkTreeArgs n = drop 1 $ concat $ take n $ map (",x"++) $ map show [1..]

    mkLine (symbol, arity) = pattern ++ " = " ++ call
      where
        pattern = prefix ++ "(" ++ name ++ symbol ++ (mkArgs arity) ++ ")"
        call = "[\"" ++ symbol ++ "\"]" ++ " ++ drawSubTrees [" ++ (mkTreeArgs arity) ++ "]"

    topLines = [ "draw" ++ name ++ " :: (Show a) => " ++ name ++ " a -> [String]"
               , "draw" ++ name ++ " = go"
               , "  where"
               , "    go (" ++ name ++ "Var x) = [show x]"
               ]
    
    logicLines = [ mkLine ("Eq", 2)
                , mkLine ("And", 2)
                , mkLine ("Or", 2)
                , mkLine ("Implies", 2)
                , mkLine ("Iff", 2)
                , mkLine ("Not", 1)
                , "    go (" ++ name ++ "ForAll x1 x2) = [\"For all \" ++ show x1] ++ drawSubTrees [x2]"
                , "    go (" ++ name ++ "Exists x1 x2) = [\"For all \" ++ show x1] ++ drawSubTrees [x2]"
                ]

    constLines = fmap (\c -> prefix ++ "(" ++ name ++ c ++ ") = [\"" ++ c ++ "\"]") cs
    relLines   = fmap mkLine rs
    funLines   = fmap mkLine fs

    bottomLines = [ ""
                  , "    drawSubTrees []     = []"
                  , "    drawSubTrees [t]    = \"|\" : shift \"`- \" \"   \" (draw" ++ name ++ " t)"
                  , "    drawSubTrees (t:ts) = \"|\" : shift \"+- \" \"|  \" (draw" ++ name ++ " t) ++ drawSubTrees ts"
                  , ""
                  , "    shift first other = zipWith (++) (first : repeat other)"
                  , ""
                  , "instance (Show a) => Show (" ++ name ++ " a) where"
                  , "  show = unlines . draw" ++ name
                  ]

    full = [topLines, logicLines, constLines, relLines, funLines, bottomLines]

makeST :: String -> [String] -> [(String, Int)] -> [(String, Int)] -> String
makeST name cs rs fs = (makeData name cs rs fs) ++ "\n\n" ++ (makePrettyPrinter name cs rs fs)

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
               , "      v <- indentifier lexer" ++ name
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

        mkDefn (f,n) = [ "    parse" ++ name ++ f ++ " do" 
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

    otherDefinitions = concat $ fmap mkDefn rs
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
                 , "parse" ++ name ++ "' = (parens" ++ lexName ++ " parse" ++ name ++") <|> parseForAll <|> parseExists <|> parseAtom"
                 , "  where"
                 , "    parseAtom = do"
                 , "      x <- parse" ++ name ++ "Atomic"
                 , "      return $ " ++ name ++ "Atom x"
                 , ""
                 , "    parseForAll = do"
                 , "      reserved " ++ lexName ++ " \"ForAll\""
                 , "      x <- identifier " ++ lexName
                 , "      e <- parens " ++ lexName ++ " parse" ++ name
                 , "      return $ " ++ name ++ "Exists x e"
                 ]

-- }}}

