{-# LANGUAGE DeriveFunctor #-}
module Main where
import System.IO

-- {{{ Extremely Basic Set Theory:
-- Minimal FOL, with membership
data EBST a = EBSTVar a
            | EBSTEq  (EBST a) (EBST a)
            | EBSTAnd (EBST a) (EBST a)
            | EBSTNot (EBST a)
            | EBSTForAll a (EBST a)
            | EBSTIn  (EBST a) (EBST a)
  deriving (Functor)

drawEBST :: (Show a) => EBST a -> [String]
drawEBST = go
  where
    go (EBSTVar x) = [show x]
    go (EBSTEq x y) = ["="] ++ drawSubTrees [x,y]
    go (EBSTIn x y) = ["In"] ++ drawSubTrees [x,y]
    go (EBSTAnd x y) = ["And"] ++ drawSubTrees [x,y]
    go (EBSTNot x) = ["Not"] ++ drawSubTrees [x]
    go (EBSTForAll x y) = ["For All " ++ show x] ++ drawSubTrees [y]

    drawSubTrees [] = []
    drawSubTrees [t] = "|" : shift "`- " "   " (drawEBST t)
    drawSubTrees (t:ts) = "|" : shift "+- " "|  " (drawEBST t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

instance (Show a) => Show (EBST a) where
  show = unlines . drawEBST

--}}}

-- {{{ Slightly Less Basic Set Theory:
-- All FOL, with membership
data SLBST a = SLBSTVar a
             | SLBSTEq  (SLBST a) (SLBST a)
             | SLBSTAnd (SLBST a) (SLBST a)
             | SLBSTOr  (SLBST a) (SLBST a)
             | SLBSTTo  (SLBST a) (SLBST a)
             | SLBSTIff (SLBST a) (SLBST a)
             | SLBSTNot (SLBST a)
             | SLBSTForAll a (SLBST a)
             | SLBSTExists a (SLBST a)
             | SLBSTIn  (SLBST a) (SLBST a)
  deriving (Functor)

drawSLBST :: (Show a) => SLBST a -> [String]
drawSLBST = go
  where
    go (SLBSTVar x) = [show x]
    go (SLBSTEq x y) = ["="] ++ drawSubTrees [x,y]
    go (SLBSTIn x y) = ["In"] ++ drawSubTrees [x,y]
    go (SLBSTAnd x y) = ["And"] ++ drawSubTrees [x,y]
    go (SLBSTOr x y) = ["Or"] ++ drawSubTrees [x,y]
    go (SLBSTTo x y) = ["Implies"] ++ drawSubTrees [x,y]
    go (SLBSTIff x y) = ["Iff"] ++ drawSubTrees [x,y]
    go (SLBSTNot x) = ["Not"] ++ drawSubTrees [x]
    go (SLBSTForAll x y) = ["For all " ++ show x] ++ drawSubTrees [y]
    go (SLBSTExists x y) = ["There exists " ++ show x] ++ drawSubTrees [y]

    drawSubTrees [] = []
    drawSubTrees [t] = "|" : shift "`- " "   " (drawSLBST t)
    drawSubTrees (t:ts) = "|" : shift "+- " "|  " (drawSLBST t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

instance (Show a) => Show (SLBST a) where
  show = unlines . drawSLBST

-- }}}

-- {{{ Proper Set Theory:
-- 

--}}}





slbstToEbst :: SLBST a -> EBST a
slbstToEbst (SLBSTVar x)      = 
  EBSTVar x
slbstToEbst (SLBSTEq x y)     = 
  EBSTEq (slbstToEbst x) (slbstToEbst y) 
slbstToEbst (SLBSTIn x y)     = 
  EBSTIn (slbstToEbst x) (slbstToEbst y) 
slbstToEbst (SLBSTAnd x y)    = 
  EBSTAnd (slbstToEbst x) (slbstToEbst y) 
slbstToEbst (SLBSTOr x y)     = 
  EBSTNot (EBSTAnd (EBSTNot (slbstToEbst x)) (EBSTNot (slbstToEbst y)))
slbstToEbst (SLBSTTo x y)     = 
  EBSTNot (EBSTAnd (slbstToEbst x) (EBSTNot (slbstToEbst y)))
slbstToEbst (SLBSTIff x y)    = 
  EBSTAnd 
  (EBSTNot (EBSTAnd (slbstToEbst x) (EBSTNot (slbstToEbst y)))) 
  (EBSTNot (EBSTAnd (slbstToEbst y) (EBSTNot (slbstToEbst x))))
slbstToEbst (SLBSTNot x)      = 
  EBSTNot (slbstToEbst x)
slbstToEbst (SLBSTForAll x y) = 
  EBSTForAll x (slbstToEbst y)
slbstToEbst (SLBSTExists x y) = 
  EBSTNot (EBSTForAll x (EBSTNot (slbstToEbst y)))

extensionality :: SLBST String
extensionality = 
  SLBSTForAll "x" 
  (
    SLBSTForAll "y"
    (
      SLBSTTo
      ( SLBSTForAll "z" (SLBSTIff (SLBSTIn z x) (SLBSTIn z y)) )
      ( SLBSTEq x y )
    )
  )
  where
    x = SLBSTVar "x"
    y = SLBSTVar "y"
    z = SLBSTVar "z"

main :: IO ()
main = undefined
