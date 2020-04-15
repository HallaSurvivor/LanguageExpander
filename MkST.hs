-- A module to make ASTs for Main.hs
module MkST where

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
                , mkLine ("ForALl a", 1)
                , mkLine ("Exists a", 1)
                ]

    constLines = fmap (\c -> mkLine (c,0)) cs
    relLines   = fmap mkLine rs
    funLines   = fmap mkLine fs

    bottomLine = ["  deriving (Functor)"]
  
makePrettyPrinter name cs rs fs = unlines $ concat full
  where
    prefix = "    go "

    mkArgs n = concat $ take n $ map (" x"++) $ map show [1..]
    mkTreeArgs n = drop 1 $ concat $ take n $ map (",x"++) $ map show [1..]

    mkLine (symbol, arity) = pattern ++ " = " ++ call
      where
        pattern = prefix ++ "(" ++ name ++ symbol ++ (mkArgs arity) ++ ")"
        call = "[\"" ++ symbol ++ "\"]" ++ " ++ drawSubTrees [" ++ (mkTreeArgs arity) ++ "]"

    topLines = [ "draw" ++ name ++ " :: (Show a) => " ++ name ++ " a -> String"
               , "draw" ++ name ++ " = unlines . go"
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

    constLines = fmap (\c -> mkLine (c,0)) cs
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
                  , "  show = draw" ++ name
                  ]

    full = [topLines, logicLines, constLines, relLines, funLines, bottomLines]
