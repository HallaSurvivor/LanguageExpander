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

  let parser = mkLexer name cs rs fs ++ "\n" ++
               mkTermParser name cs rs fs ++ "\n" ++
               mkAtomicParser name cs rs fs ++ "\n" ++
               mkParser name cs rs fs ++ "\n"

  putStrLn syntaxTree

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

