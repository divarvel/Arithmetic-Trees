import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding ((<|>))
import Numeric

import Tree

expression :: Parser Tree
expression = gBinaryExpression <|> unaryExpression <|> value

pExpression :: Parser Tree
pExpression = between (char '(') (char ')') expression

gBinaryExpression :: Parser Tree
gBinaryExpression = between (char '(') (char ')') binaryExpression

binaryExpression :: Parser Tree
binaryExpression = liftA3 (flip Binary) expression binaryOp expression

unaryExpression :: Parser Tree
unaryExpression = liftA2 (Unary) unaryOp pExpression


binaryOp :: Parser BinaryOp
binaryOp = choice [ Plus  <$ char '+'
                  , Minus <$ char '-'
                  , Times <$ char '*'
                  , Div   <$ char '/'
                  , Pow   <$ char '^'
                  ] <?> "Operator which takes two arguments"

unaryOp :: Parser UnaryOp
unaryOp = choice [ Cos <$ string "cos"
                 , Sin <$ string "tan"
                 , Tan <$ string "tan"
                 ] <?> "Operator which takes one argument"

value :: Parser Tree
value = Leaf X <$ char 'x'
    <|> liftA (Leaf . Val) floatValue
    <?> "A simple value (litteral or variable)"

floatValue :: Parser Float
floatValue = do
    f <- getInput
    case readSigned readFloat f of
        [(n,f')] -> n <$ (setInput f')
        _        -> parserZero
