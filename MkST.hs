-- A module to make ASTs for Main.hs
module MkST where
import System.IO
import System.Environment
import Data.List
import Data.Maybe (catMaybes, fromJust)
import Data.Char (isUpper)
import qualified Data.Map.Strict as M
import Control.Monad.State
import Text.Printf
import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr


main :: IO ()
main = do
    args <- getArgs
    case args of
      []    -> putStrLn $ "Write a help message?" -- print syntax, etc?
      (x:_) -> do
        s <- readFile x
        -- Turn the file into a [[String]], a list of definition blocks
        let ss = filter (\l -> length l > 1) $ groupBy (\x y -> x /= "" && y /= "") $ lines s
        ts <- sequence $ fmap parseBlock ss
        let theoryMap = M.fromList [(_name t, t) | t <- ts]
        let ts' = evalState (mapM addDerivations ts) theoryMap
        writeFile "Converter.hs" (boilerplate ts' ++ concatMap mkST ts')
  where
    mkST :: Theory -> String
    mkST t = unlines $ 
      [ printf "-- {{{ %s" (_name t)
      , mkDataTypes t
      , mkPrettyPrinter t
      , mkLexer t
      , mkParsers t
      , mkConverters t
      , "--}}}"
      , ""
      ]

addDerivations :: Theory -> State (M.Map String Theory) Theory
addDerivations t = do
    tm <- get
    let es   = fmap (tm M.!?) (_extending t)
    let es'  = catMaybes es -- one day maybe we'll thread errors around
    let tNew = t <> foldMap derived es'
    put $ M.insert (_name t) tNew tm
    return tNew
  where
    derived t = mempty { _derivedConstants = _constants t ++ _derivedConstants t 
                       , _derivedFunctions = _functions t ++ _derivedFunctions t
                       , _derivedRelations = _relations t ++ _derivedRelations t
                       }

-- {{{ Parse the input file

data Theory = Theory { _name             :: String
                     , _extending        :: [String]
                     , _constants        :: [String]
                     , _functions        :: [(String, Int)]
                     , _relations        :: [(String, Int)]
                     , _constDefns       :: [(String, String)]
                     , _funDefns         :: [(String, String)]
                     , _relDefns         :: [(String, String)]
                     , _derivedConstants :: [String]
                     , _derivedFunctions :: [(String,Int)]
                     , _derivedRelations :: [(String,Int)]
                     } deriving Show

instance Semigroup Theory where
  t <> s = Theory
    (_name t       <> _name s)
    (_extending t  <> _extending s)
    (_constants t  <> _constants s)
    (_functions t  <> _functions s)
    (_relations t  <> _relations s)
    (_constDefns t <> _constDefns s)
    (_funDefns t   <> _funDefns s)
    (_relDefns t   <> _relDefns s)
    (_derivedConstants t <> _derivedConstants s)
    (_derivedFunctions t <> _derivedFunctions s)
    (_derivedRelations t <> _derivedRelations s)

instance Monoid Theory where
  mempty = Theory "" [] [] [] [] [] [] [] [] [] []


word :: Parser String
word = many1 letter

nameParser :: Parser Theory
nameParser = do
  name <- word
  char ':'
  spaces
  eof
  return $ mempty {_name = name}

extendParser :: Parser Theory
extendParser = do
  string "extending"
  spaces
  st <- word
  spaces
  eof
  return $ mempty {_extending = [st]}

constNewParser :: Parser Theory
constNewParser = do
  string "constNew"
  spaces
  c <- word
  spaces
  eof
  return $ mempty {_constants = [c]}

funNewParser :: Parser Theory
funNewParser = do
  string "funNew"
  spaces
  char '('
  spaces
  f <- word
  spaces
  char ','
  spaces
  n <- many1 digit
  spaces
  char ')'
  spaces
  eof
  return $ mempty {_functions = [(f,read n)]}

relNewParser :: Parser Theory
relNewParser = do
  string "relNew"
  spaces
  char '('
  spaces
  r <- word
  spaces
  char ','
  spaces
  n <- many1 digit
  spaces
  char ')'
  spaces
  eof
  return $ mempty {_relations = [(r,read n)]}

constDefParser :: Parser Theory
constDefParser = do
  string "constDef"
  spaces
  c <- word
  spaces
  d <- many1 anyChar
  eof
  return $ mempty {_constants = [c], _constDefns = [(c,d)]}

funDefParser :: Parser Theory
funDefParser = do
  string "funDef"
  spaces
  char '('
  spaces
  f <- word
  spaces
  char ','
  spaces
  n <- many1 digit
  spaces
  char ')'
  spaces
  d <- many1 anyChar
  eof
  return $ mempty {_functions = [(f,read n)], _funDefns = [(f,d)]}

relDefParser :: Parser Theory
relDefParser = do
  string "relDef"
  spaces
  char '('
  spaces
  r <- word
  spaces
  char ','
  spaces
  n <- many1 digit
  spaces
  char ')'
  spaces
  d <- many1 anyChar
  eof
  return $ mempty {_relations = [(r,read n)], _relDefns = [(r,d)]}

lineParser :: Parser Theory
lineParser =  try nameParser
          <|> try extendParser
          <|> try constNewParser
          <|> try funNewParser
          <|> try relNewParser
          <|> try constDefParser
          <|> try funDefParser
          <|> try relDefParser

parseBlock :: [String] -> IO Theory
parseBlock s = fmap mconcat $ sequence $ fmap parseLine s
  where
    parseLine l = case (parse lineParser "" l) of
      Left  e -> putStrLn ("error: " ++ (show e)) >> return mempty
      Right v -> return v

-- }}}

-- {{{ Top Level Stuff

boilerplate :: [Theory] -> String
boilerplate ts = 
    unlines $ [ "{-# LANGUAGE FlexibleInstances #-}"
              , "{-# LANGUAGE UndecidableInstances #-}"
              , "module Converter where"
              , "import Text.Parsec hiding (State)"
              , "import Text.Parsec.String"
              , "import Text.Parsec.Token"
              , "import Text.Parsec.Language"
              , "import Text.Parsec.Expr"
              , "import Control.Monad.State"
              , "import qualified Data.Map as M"
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
              , "type Context = ([String], M.Map String String)"
              , ""
              , "ctx0 :: Context"
              , "ctx0 = ([\"x\" ++ show i | i <- [1..]], M.empty)"
              , ""
              , "fresh :: State Context String"
              , "fresh = do"
              , "  (v:vs, m) <- get"
              , "  put (vs, m)"
              , "  return v"
              , ""
              , "addVar :: String -> State Context String"
              , "addVar s = do"
              , "  v <- fresh"
              , "  modify (\\(vs,m) -> (vs, M.insert s v m))"
              , "  return v"
              , ""
              , "getVar :: String -> State Context String"
              , "getVar s = do"
              , "  (_,m) <- get"
              , "  return $ m M.! s -- make this fail gracefully"
              , ""
              , ""
              ] ++ parseInput ++
              [ ""
              , "convert :: String -> String"
              , "convert s ="
              , "  case ret of"
              , "    Left e -> \"error: \" ++ (show e)"
              , "    Right v -> v"
              , "  where"
              , "    ret = parse parseInput \"\" s"
              , ""
              , "main :: IO ()"
              , "main = interact (unlines . (map convert) . lines)"
              , ""
              ]
  where
    mkInput (0,t) = printf "      try parse%sToString" (_name t)
    mkInput (_,t) = printf "  <|> try parse%sToString" (_name t)

    parseInput = [ "parseInput :: Parser String"
                 , "parseInput = do"
                 ] ++ (fmap mkInput $ zip [0..] ts)


-- }}}

-- {{{ Make the datatype

mkDataTypes :: Theory -> String
mkDataTypes t = unlines $ [terms, atomics, formulas]
  where
    (name, cs, fs, rs) = ( _name t
                         , _derivedConstants t ++ _constants t
                         , _derivedFunctions t ++ _functions t
                         , _derivedRelations t ++ _relations t
                         )

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
      , printf "    %sTrue" name
      , printf "  | %sFalse" name
      , printf "  | %sEq (%sTerm a) (%sTerm a)" name name name
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
  
mkPrettyPrinter :: Theory -> String
mkPrettyPrinter t = unlines $ [terms, atomics, formulas, showDefns]
  where
    (name, cs, fs, rs) = ( _name t
                         , _derivedConstants t ++ _constants t
                         , _derivedFunctions t ++ _functions t
                         , _derivedRelations t ++ _relations t
                         )

    mkLine (symbol, 0) = 
        printf "    go (%s%s) = [\"%s\"]" name symbol symbol
    mkLine (symbol, arity) = 
        printf "    go (%s%s%s) = [\"%s\"] ++ draw [%s]" name symbol args symbol treeArgs
      where
        args = concat $ take arity $ map (" x"++) $ map show [1..]
        treeArgs = drop 1 $ concat $ take arity $ map (",x"++) $ map show [1..]

    terms = unlines $
      [ printf "draw%sTerm :: (Show a) => %sTerm a -> [String]" name name
      , printf "draw%sTerm = go" name
      , "  where"
      , printf "    draw :: (Show a) => [%sTerm a] -> [String]" name
      , printf "    draw = drawGenericSubTree draw%sTerm" name
      , printf "    go (%sVar x) = [show x]" name
      ] ++
      (fmap (\c -> printf "    go (%s%s) = [\"%s\"]" name c c) cs) ++ 
      fmap mkLine fs

    atomics = unlines $
      [ printf "draw%sAtomic :: (Show a) => %sAtomic a -> [String]" name name
      , printf "draw%sAtomic = go" name
      , "  where"
      , printf "    draw :: (Show a) => [%sTerm a] -> [String]" name
      , printf "    draw = drawGenericSubTree draw%sTerm" name
      ] ++
      (fmap mkLine $ ("True", 0) : ("False", 0) : ("Eq", 2) : rs)

    formulas = unlines
      [ printf "draw%s :: (Show a) => %s a -> [String]" name name
      , printf "draw%s = go" name
      , "  where"
      , printf "    draw :: (Show a) => [%s a] -> [String]" name
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

mkLexer :: Theory -> String
mkLexer t = unlines $ 
    [ "lexer" ++ name ++ " :: TokenParser ()"
    , "lexer" ++ name ++ " = makeTokenParser languageDef"
    , "  where"
    , "    languageDef ="
    , "      emptyDef { identStart = letter"
    , "               , identLetter = alphaNum"
    , "               , reservedNames = [ \"True\""
    , "                                 , \"False\""
    , "                                 , \"Eq\""
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
  where
    (name, cs, fs, rs) = ( _name t
                         , _derivedConstants t ++ _constants t
                         , _derivedFunctions t ++ _functions t
                         , _derivedRelations t ++ _relations t
                         )

mkParsers :: Theory -> String
mkParsers t = unlines $ [terms, atomics, formulas, exposed]
  where
    (name, cs, fs, rs) = ( _name t
                         , _derivedConstants t ++ _constants t
                         , _derivedFunctions t ++ _functions t
                         , _derivedRelations t ++ _relations t
                         )

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

    mkDefn (symbol, 0) = 
      [ printf "    parse%s%s = do" name symbol
      , printf "      reserved lexer%s \"%s\"" name symbol
      , printf "      return %s%s" name symbol
      , ""
      ]
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
      (concat $ fmap mkDefn $ ("True",0) : ("False",0) : ("Eq", 2):rs)

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
      , ""
      ,        "    parseExists = do"
      , printf "      reserved lexer%s \"Exists\"" name
      , printf "      x <- identifier lexer%s" name
      , printf "      e <- parens lexer%s parse%s" name name
      , printf "      return $ %sExists x e" name
      ]

    exposed = unlines $
      [ printf "parse%sToString :: Parser String" name
      , printf "parse%sToString = do" name
      , printf "  string \"%s\"" name
      ,        "  spaces"
      , printf "  tree <- parse%s" name
      , printf "  eof"
      ,        "  return $ show tree"
      ]

-- }}}

-- {{{ Make the converters

data Term a = Var a
            | Const a 
            | Fun a [Term a] 
            | Output 
            | Input Int
            deriving Show

data Atomic a = True'
              | False'
              | Eq (Term a) (Term a) 
              | Rel a [Term a]
              deriving Show

data Expr a = Atom (Atomic a) 
            | And (Expr a) (Expr a)
            | Or (Expr a) (Expr a)
            | Implies (Expr a) (Expr a)
            | Iff (Expr a) (Expr a)
            | Not (Expr a)
            | ForAll a (Expr a)
            | Exists a (Expr a)
            deriving Show

lexer :: TokenParser ()
lexer = makeTokenParser languageDef
  where
    languageDef =
      emptyDef { identStart = letter
               , identLetter = alphaNum
               , reservedNames = [ "True"
                                 , "False"
                                 , "Eq"
                                 , "Not"
                                 , "ForAll"
                                 , "Exists"
                                 , "o"
                                 ]
               , reservedOpNames = [ "&&", "||", "->", "<->" ]
               }

parseTerm :: Parser (Term String)
parseTerm = try parseOut <|> try parseIn <|> try parseFun <|> try parseVar
  where
    parseOut = do
      reserved lexer "o"
      return Output

    parseIn = do
      char 'v'
      n <- natural lexer
      return $ Input $ fromInteger n

    parseFun = do
      f <- identifier lexer
      char '('
      whiteSpace lexer
      t <- parseTerm
      ts <- many $ char ',' >> whiteSpace lexer >> parseTerm
      char ')'
      whiteSpace lexer
      return $ Fun f (t:ts)

    parseVar = do
      v <- identifier lexer
      case (isUpper (v!!0)) of
        True  -> return $ Const v
        False -> return $ Var v
    
parseAtomic :: Parser (Atomic String)
parseAtomic = try parseTrue <|> try parseFalse <|> try parseEq <|> try parseRel
  where
    parseTrue = do
      reserved lexer "True"
      return True'

    parseFalse = do
      reserved lexer "False"
      return False'

    parseEq = do
      reserved lexer "Eq"
      char '('
      whiteSpace lexer
      x1 <- parseTerm
      char ','
      whiteSpace lexer
      x2 <- parseTerm
      char ')'
      whiteSpace lexer
      return $ Eq x1 x2

    parseRel = do
      r <- identifier lexer
      char '('
      whiteSpace lexer
      t <- parseTerm
      ts <- many $ char ',' >> whiteSpace lexer >> parseTerm
      char ')'
      whiteSpace lexer
      return $ Rel r (t:ts)

parseExpr :: Parser (Expr String)
parseExpr = (flip buildExpressionParser) parseExpr' $ [
    [ Prefix (reserved lexer "Not" >> return Not) ]
  , [ Infix (reservedOp lexer "&&" >> return And) AssocLeft ]
  , [ Infix (reservedOp lexer "||" >> return Or) AssocLeft ]
  , [ Infix (reservedOp lexer "->" >> return Implies) AssocRight
    , Infix (reservedOp lexer "<->" >> return Iff) AssocLeft
    ]
  ]
      
parseExpr' :: Parser (Expr String)
parseExpr' = (parens lexer parseExpr) <|> parseForAll <|> parseExists <|> parseAtom
  where
    parseAtom = do
      x <- parseAtomic
      return $ Atom x

    parseForAll = do
      reserved lexer "ForAll"
      x <- identifier lexer
      e <- parens lexer parseExpr
      return $ ForAll x e

    parseExists = do
      reserved lexer "Exists"
      x <- identifier lexer
      e <- parens lexer parseExpr
      return $ Exists x e
    
useParser :: String -> (Expr String)
useParser s =
  case ret of 
    -- Left  e -> handle errors gracefully
    Right v -> v
  where
    ret = parse parseExpr' "" s

mkConverters :: Theory -> String
mkConverters t = unlines $ [ classDefn, inheritance, instances ]
  where
    name = _name t
    extending = _extending t

    classDefn = unlines $
      [ printf "class ConvertibleTo%s t where" name
      , printf "  convertTo%s :: t a -> %s a" name name
      , ""
      ]

    inheritance = unlines $ fmap mkInheritance extending

    mkInheritance e = unlines $
      [ printf "instance {-# OVERLAPPABLE #-} (ConvertibleTo%s t) => ConvertibleTo%s t where" name e
      , printf "  convertTo%s = convertTo%s . convertTo%s" e e name
      ]

    instances = unlines $ fmap mkInstance extending

    mkInstance e = unlines $
        [ printf "instance ConvertibleTo%s %s where" e name
        , printf "  convertTo%s = (flip evalState ctx0) . convertExpr" e
        ,        "    where"
        , printf "      convertExpr (%sAtom x)      = convertAtomic x" name
        , printf "      convertExpr (%sAnd x y)     = %sAnd      <$> (convertExpr x) <*> (convertExpr y)" name e
        , printf "      convertExpr (%sOr x y)      = %sOr       <$> (convertExpr x) <*> (convertExpr y)" name e
        , printf "      convertExpr (%sImplies x y) = %sImplies  <$> (convertExpr x) <*> (convertExpr y)" name e
        , printf "      convertExpr (%sIff x y)     = %sIff      <$> (convertExpr x) <*> (convertExpr y)" name e
        , printf "      convertExpr (%sNot x)       = %sNot      <$> (convertExpr x)" name e
        , printf "      convertExpr (%sForAll a x)  = %sForAll a <$> (convertExpr x)" name e
        , printf "      convertExpr (%sExists a x)  = %sExists a <$> (convertExpr x)" name e
        , ""
        , printf "      foldAtomic a ds = %sAnd (%sAtom a) (foldr %sAnd (%sAtom %sTrue) ds)" e e e e e
        ] ++ atomicDefns ++
        [ ""
        ,        "      convertTerm = undefined"
        ]
      where
        atomicDefns = concat [ fmap mkDerivedRel (("True",0) : ("False",0) : ("Eq",2) : (_derivedRelations t))
                             , fmap mkDefinedRel (_relDefns t) 
                             ]

        termDefns = concat [ fmap mkDerivedFun (_derivedFunctions t)
                           , fmap mkDefinedFun (_funDefns t)
                           , fmap mkDerivedConst (_derivedConstants t)
                           , fmap mkDefinedConst (_constDefns t)
                           ]

        args :: Int -> String
        args  n = concat $ [printf " x%d"  i | i <- [1..n]]

        args' :: Int -> String
        args' n = concat $ [printf " x%d'" i | i <- [1..n]]

        mkDerivedRel :: (String, Int) -> String
        mkDerivedRel (r,n) = unlines $ 
               [ printf "      convertAtomic (%s%s%s) = do" name r (args n) ] 
            ++ ( fmap (\i -> printf "        (x%d', dx%d) <- convertTerm x%d" i i i) [1..n] )
            ++ [ printf "        let ds = concat [%s]" (concat $ intersperse ", " $ fmap (printf "dx%d") [1..n])
               , printf "        return $ foldAtomic (%s%s%s) ds" e r (args' n)
               ]
            
        mkDerivedFun :: (String, Int) -> String
        mkDerivedFun (f,n) = printf "      convertTerm (%s%s%s) = %s%s%s" name f (args n) e f convertedArgs
          where
            convertedArgs = concat $ take n $ zipWith (\s n -> s ++ show n ++ ")") (repeat $ " (convertTerm x") [1..]

        mkDerivedConst :: String -> String
        mkDerivedConst c = printf "      convertTerm %s%s = %s%s" name c e c

        getIn :: (Eq a) => Expr a -> Int
        getIn (And x y)     = max (getIn x) (getIn y)
        getIn (Or x y)      = max (getIn x) (getIn y)
        getIn (Implies x y) = max (getIn x) (getIn y)
        getIn (Iff x y)     = max (getIn x) (getIn y)
        getIn (Not x)       = getIn x
        getIn (ForAll a x)  = getIn x
        getIn (Exists a x)  = getIn x
        getIn (Atom x)      = getIn' x
          where
            getIn' (Eq x y)   = max (getIn'' x) (getIn'' y)
            getIn' (Rel a xs) = maximum $ fmap getIn'' xs

            getIn'' (Input n)  = n
            getIn'' (Fun f xs) = maximum $ fmap getIn'' xs
            getIn'' _          = 0


        mkDefinedRel :: (String, String) -> String
        mkDefinedRel (r,d) = printf "      convertAtomic (%s%s%s) = %s" name r (args n) converted
          where
            t = useParser d
            n = getIn t
            converted = "undefined"
            
        mkDefinedFun :: (String, String) -> String
        mkDefinedFun (f,d) = printf "      convertTerm (%s%s%s) = %s" name f (args n) converted
          where
            t = useParser d
            n = getIn t
            converted = "undefined"

        mkDefinedConst :: (String, String) -> String
        mkDefinedConst (c,d) = printf "      convertTerm %s%s = %s" name c converted
          where
            t = useParser d
            converted = "undefined"

-- }}}
