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
  
  

