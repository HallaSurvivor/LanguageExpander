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
      

-- }}}

