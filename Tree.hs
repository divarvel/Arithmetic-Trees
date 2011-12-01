module Tree where

import Data.List

data BinaryOp = Plus | Minus | Times | Div | Pow

instance Show BinaryOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Pow = "^"

applyBin :: BinaryOp -> Float -> Float -> Float
applyBin Plus  a b = a + b
applyBin Minus a b = a - b
applyBin Times a b = a * b
applyBin Div   a b = a / b
applyBin Pow a b  = a ^ (truncate b)

deriveBin :: BinaryOp -> Tree -> Tree -> Tree
deriveBin Plus l r = Binary Plus (derive l) (derive r)
deriveBin Minus l r = Binary Minus (derive l) (derive r)
deriveBin Times l r = Binary Plus (Binary Times (derive l) r) (Binary Times l (derive r))
deriveBin Div l r = Binary Div 
                        (Binary Plus (Binary Times (derive l) r) (Binary Times l (derive r)))
                        (Binary Pow (r) (Leaf (Val 2)))
deriveBin Pow l (Leaf (Val v)) = Binary Times 
                        (Binary Times (Leaf (Val v)) 
                          (Binary Pow l (Leaf (Val (v - 1)))) 
                        )
                        (derive l)
deriveBin Pow _ _ = undefined


data UnaryOp = Sin | Cos | Tan deriving Show

applyUn :: UnaryOp -> Float -> Float
applyUn Sin = sin
applyUn Cos = cos
applyUn Tan = tan

deriveUn :: UnaryOp -> Tree -> Tree 
deriveUn Sin t = Binary Times
                   (Unary Cos t)
                   (derive t)
deriveUn Cos t = Binary Times
                   (Binary Times
                     (Leaf (Val (-1))) 
                     (Unary Sin t))
                   (derive t)
deriveUn Tan t = Binary Times
                   (Binary Plus
                     (Leaf (Val 1)) 
                     (Binary Pow
                       (Unary Tan t)
                       (Leaf (Val 2)) 
                     )
                   ) 
                   (derive t)

data Value = X | Val Float

instance Show Value where
  show X = "x"
  show (Val v) = show v

applyV :: Value -> Float
applyV X = undefined
applyV (Val a) = a

deriveV :: Value -> Tree
deriveV X = Leaf (Val 1)
deriveV (Val _) = Leaf (Val 0)

data Tree = Leaf Value | Binary BinaryOp Tree Tree | Unary UnaryOp Tree

instance Show Tree where
  show (Binary op l r) = "(" ++ (intercalate " " [show l, show op, show r]) ++ ")"
  show (Unary op t) = show op ++ "(" ++ show t ++ ")"
  show (Leaf v) = show v

eval :: Tree -> Float
eval (Leaf v) = applyV v 
eval (Unary f t) = applyUn f $ eval t
eval (Binary f t1 t2) = applyBin f (eval t1) (eval t2)

derive :: Tree -> Tree
derive (Leaf v) = deriveV v
derive (Unary op t) = deriveUn op t 
derive (Binary op l r) = deriveBin op l r

replace :: Tree -> Float -> Tree
replace (Leaf X) value = Leaf (Val value)
replace (Leaf (Val v)) _ = Leaf (Val v)
replace (Unary f t) value = Unary f (replace t value)
replace (Binary f l r) value = Binary f (replace l value) (replace r value)

clean :: Tree -> Tree
clean (Binary Plus l (Leaf (Val 0))) = clean l
clean (Binary Plus (Leaf (Val 0)) r) = clean r
clean (Binary Minus l (Leaf (Val 0))) = clean l

clean (Binary Times (Leaf (Val 0)) _) = Leaf (Val 0)
clean (Binary Times _ (Leaf (Val 0))) = Leaf (Val 0)
clean (Binary Times l (Leaf (Val 1))) = clean l
clean (Binary Times (Leaf (Val 1)) r) = clean r
clean (Binary Div (Leaf (Val 0)) _) = Leaf (Val 0)
clean (Binary Div l (Leaf (Val 1))) = clean l
clean (Binary op l r) = Binary op (clean l) (clean r)
clean (Unary op t) = Unary op (clean t)
clean (Leaf v) = Leaf v

