https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module GoatParser (ast)
where

-------------------------------------------------------------------------
--  A parser for Goat, poorly commented.
--  It should still be fairly easy to read, as it uses the Parsec parser
--  library.
--
--  Harald Sondergaard, April 2019
-------------------------------------------------------------------------

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

data MaybeOneOrTwo a
  = None
  | One a
  | Two a a
  | TooMany
    deriving (Eq, Show)

type Parser a
   = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
   = Q.makeTokenParser
     (emptyDef
     { Q.commentLine     = "#"
     , Q.nestedComments  = True
     , Q.identStart      = letter
     , Q.opStart         = oneOf "+-*/=!<>&|:"
     , Q.opLetter        = oneOf "=&|"
     , Q.reservedNames   = myReserved
     , Q.reservedOpNames = myOpnames
     })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
  = [ "begin", "bool", "call", "do", "else", "end", "false"
    , "fi", "float", "if", "int", "od", "proc", "read"
    , "ref", "then", "true", "val", "while", "write"
    ]

myOpnames 
  = [ "+", "-", "*", "/"
    , "=", "!=", "<", ">", "<=", ">="
    , "&&", "||", "!", ":="
    ]

-- Translate a source code position to a pair of integers (line, column)

comps :: SourcePos -> (Int,Int)
comps pos
  = (sourceLine pos, sourceColumn pos)

pExp :: Parser Expr
pExp
  = buildExpressionParser table pFac
    <?> "expression"

