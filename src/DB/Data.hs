module DB.Data where

import MTA.Parser
import Data.Char

-- program      ::= tablecommand* query
-- tablecommand ::= create
--                | insert
-- create       ::= 'CREATE' 'TABLE' name '(' names? ')'
-- insert       ::= 'INSERT' 'INTO' name insertion
-- insertion    ::= 'VALUES' '(' entries? ')'
--                | query
-- query        ::= 'SELECT' names 'FROM' names wherepart?
-- wherepart    ::= 'WHERE' expression
-- expression   ::= expression 'AND' expression
--                | expression 'OR' expression
--                | 'NOT' expression
--                | '(' expression ')'
--                | operand operator operand
-- names        ::= name ',' names
--                | name
-- entries      ::= entry ',' entries
--                | entry
-- entry        ::= string
--                | number
-- operand      ::= name
--                | entry
-- operator     ::= '<' | '>' | '='
type Name  = String
data Entry = EntryStr String | EntryNum Int
data Operand = OperandName Name | OperandEntry Entry
data Operator = LT | GT | EQ


type Program = TableCmd
data TableCmd = TblCmdCreate Create | TblCmdInsert Insert
data Create = Create { createName :: Name, createNames :: [Name] }
data Insert = Insert { insertName :: Name, insertInsertion :: Insertion }
data Insertion = Insertion { insertionEntries :: [Entry], insertionQuery :: Query }
data Query = Query { querySelNames :: [Name], queryFromNames :: [Name], queryWherepart :: Maybe Expr }
--data Wherepart = Wherepart Expr
data Expr = And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Expr Operand Operator Operand

-- name     ::= letter alphanum*
-- string   ::= ' char* '
-- number   ::= digit digit*
-- alphanum ::= letter | digit
-- letter   ::= any letter
-- digit    ::= any digit
-- char     ::= any character except the single quote '

-- Exercise 2: The fact that whitespace can occur almost everywhere within an SQL statement makes the definition
-- of parsers slightly trickier than usual. Nevertheless, we can define our parser in a single step, without
-- the need for a separate lexical analyzer. The idea is to make most of the parsers consume additional spaces at
-- the end of the input.

spaces :: Parser String
spaces = undefined


