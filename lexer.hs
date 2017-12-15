module MoBettaLexa where

import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.


-- The following makes things simpler by setting up no additional state '()
--   and restricting to a string parser.

type Parser = Parsec () String

-- The module Megaparsec defines a function
--   `parse :: Parsec e s a -> String -> s -> Either (ParseError (Token s) e) a`
-- So we get
--   `parse :: Parser a -> String -> String -> Either (ParseError (Token String) ()) a`


-- space1 is a parser from Text.Megaparsec.Char that will consume one or more whitespaces
-- L.space is a lexical analyzer that uses its first arguement to consume space characters and ignores comments per its 2nd and 3rd arguments
-- L.skipLineComment matches its 1st argument then ignores everything to end of line
-- L.skipblockComment matches begin- and end-comment strings and ignores everything between

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that consumes space after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

data MoBettaToken
  = Identifier String
  | Constant Integer
  | StringLiteral String
  | LParen
  | RParen
  | Keyword String
  deriving (Show, Eq)

keywords = ["while", "if", "then", "else", "print", "print","message", "read"] 

-- Identifiers are defined as consisting of an alpha character followed
--  by any number of alphanumeric characters.
--  `lexeme` ignores trialing whitespace and comments. 'try' rewinds the parser
--  so that if it fails, it does not produce an error, leaving the stream
--  in its original state.
identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

identifier' :: Parser MoBettaToken
identifier' = do
		x<-identifier
		if 
			x `elem` keywords
		then
			return $ Keyword x
		else
			return $ Identifier x

keyword :: Parser String
keyword = (lexeme . try) p
	where 
	p = (:) <$> letterChar <*> many alphaNumChar
	
keyword' :: Parser MoBettaToken
keyword' = do
		x<-keyword
		if 
			x `elem` keywords
		then 
			return $ Keyword x
		else 
			return $ Identifier x
	
intConst :: Parser Integer
intConst = (lexeme . try) ic
  where
    ic = do
      x <- L.decimal -- parse a literal
      notFollowedBy letterChar -- fail if followed by a letter
      return x -- return the  result if we haven't failed

intConst' :: Parser MoBettaToken
intConst' = fmap Constant intConst

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

stringLiteral' :: Parser MoBettaToken
stringLiteral' = fmap StringLiteral stringLiteral

lparen :: Parser Char
lparen = (lexeme . try ) (char '(')

lparen' :: Parser MoBettaToken
lparen' = lparen *> return LParen

rparen :: Parser Char
rparen = (lexeme . try ) (char ')')

rparen' :: Parser MoBettaToken
rparen' = rparen *> return RParen

data Statement
 = Assign String Exp
 | While BExpr Statement
 | If BExpr Statement Statement
 | Skip
 | Print String
 | Read String
 | Msg String
 | Block [Statement]
 deriving (show)
 
data BBinOp = And | Or deriving (Show)
data BUnOp = Not deriving (Show)
 
assign :: Lexer Statement
asign = do
    ident<- identifier
    char `=`
    expr <- expression
    return $ Assign ident expr
	
block :: Lexer Statement
block = do
	b<-between lbrace rbrace (many statement)
	return $ Block b
	
while :: Lexer Statement
while = do
	string "while"
	b <- bexpr
	s <- statement
	return $ While b s 

statement :: Lexer Statement
statement = assign <|> block <|> while

expression :: Lexer Expr
expression = return $ IntConst 5