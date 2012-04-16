module Abfc.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
-- note:
-- a_p *> b_p defines parser that does a_p, discards result, does b_p, returns b's result
-- (<*) is similar but returns a's result instead.
import Control.Applicative ((*>), (<*))

-- [data types for parsed macro programs]
data KeywordIdentifier = Keyword String
    deriving (Eq, Show)

data Identifier = Ident String
    deriving (Eq, Show)

data StringLiteral = StringLit String |
        ArchStringLit String
    deriving (Eq, Show)

data Argument = IdentArg Identifier |
        StringConstantArg StringLiteral |
        IntConstantArg Int |
        StackAddressArg Int |
        CharConstantArg Char
    deriving (Eq, Show)

data Statement = CallKeyword KeywordIdentifier [Argument] |
        IfBlock Identifier [Statement] |
        WhileBlock Identifier [Statement]
    deriving (Eq, Show)

data Macro = MacroDef [Identifier] [Statement]
    deriving (Eq, Show)

-- [higher-order definitions for building parsers] 

-- make a parser from a lookup table
make_lookup_char_p :: (Char, a) -> CharParser() a
make_lookup_char_p pair = (char (fst pair)) >> (return (snd pair))

-- make a parser for things of form "head(child1, ..., childn)"
make_macro_parser :: (a -> [b] -> c) -> (CharParser () a) -> (CharParser () b) -> (CharParser() c)
make_macro_parser f head_p child_p = let
        body_p = sepEndBy child_p (ws_p >> char ',' >> ws_p)
        open_p = (char '(' >> ws_p)
        close_p = (ws_p >> char ')')
    in
        liftM2 f head_p (open_p *> body_p <* close_p)

-- make a parser for things of form "wrapper(body)" that just returns the parsed body
make_wrapper_parser :: String -> (CharParser () a) -> (CharParser () a)
make_wrapper_parser s body_p = let
        open_p = (string s >> ws_p >> char '(' >> ws_p)
        close_p = (ws_p >> char ')')
    in
        open_p *> body_p <* close_p

-- careful_choice should be used instead of choice if branches share a common
-- prefix of 1 or more characters.
-- (grim detail:
-- Parsec's default behaviour is to commit to a branch once it has consumed 1 or
-- more characters. This means if we use choice to choose between branches that
-- share a common prefix of 1 or more characters, if one of the branches initially
-- looks like it might match, but doesn't, characters are consumed, and the other
-- branches don't match properly. So instead we can map the parsers for the branches
-- through `try` combinator, which makes them return the original unconsumed input if
-- they fail.)
careful_choice xs = choice $ map try xs

-- [Parsing of whitespace (incl. Python-style comments)]

block_comment_delim = "\"\"\""
block_comment_p = (string block_comment_delim) >> (manyTill anyChar (try (string block_comment_delim)))

eol_p = char '\n'

inline_comment_p = (char '#') >> (manyTill anyChar eol_p)

comment_p = (block_comment_p <|> inline_comment_p) >> return ' '

-- n.b. ws is an abbrev. of whitespace
ws_p = many (space <|> comment_p)

-- [Parsing of keywords and identifiers]
uppercase_alpha_chars = ['A'..'Z']
alpha_chars = uppercase_alpha_chars ++ ['a'..'z']
num_chars = ['0'..'9']

allcaps_string_constant_p = let
        head_chars = uppercase_alpha_chars
        tail_chars = uppercase_alpha_chars ++ num_chars ++ ['_']
    in
        liftM2 (:) (oneOf head_chars) (many $ oneOf tail_chars)

keyword_identifier_p = liftM Keyword allcaps_string_constant_p

identifier_p :: CharParser () Identifier
identifier_p = liftM Ident string_constant_p

-- [Parsing of single-quoted string literals]

escaped_char_lookup = [('n', '\n'), ('t', '\t'), ('\'', '\''), ('\"', '\"'), ('\\', '\\')]

single_quote_string_char_p :: CharParser() Char
single_quote_string_char_p = let
        unescape_char_p = noneOf ['\\', '\'']
        escape_char_p = ((char '\\') >> (choice $ map make_lookup_char_p escaped_char_lookup))
    in
        unescape_char_p <|> escape_char_p

string_constant_p :: CharParser () String
string_constant_p =
    char '\'' *> (many $ single_quote_string_char_p) <* char '\''

-- [Parsing of single-quoted character literals]

char_constant_p = 
    char '\'' *> single_quote_string_char_p <* char '\''

-- [Parsing of integer literals]

digit_lookup :: [(Char, Int)]
digit_lookup = [('0', 0), ('1', 1), ('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8), ('9', 9)]

digit_p = choice $ map make_lookup_char_p digit_lookup

sign_p :: CharParser () Int
sign_p = ((char '-') >> return (-1)) <|> return 1

int_from_digits digits = let
        base :: Int
        base = 10
    in
        foldl (\acc -> \x -> (acc * base + x)) 0 digits

int_constant_p :: CharParser () Int
int_constant_p = let
        digits_p = many1 digit_p
    in
        liftM2 (*) sign_p (liftM int_from_digits digits_p)

-- [Parsing of arch.XYZ constants]

-- n.b. this part of the grammar is a bit of a hack...

arch_constant_p = liftM ArchStringLit $ string "arch." *> allcaps_string_constant_p

-- [Parsing of arguments]

arg_p = careful_choice [identifier_arg_p, int_arg_p, stack_arg_p, string_arg_p, char_arg_p]

string_arg_p = liftM StringConstantArg $ make_wrapper_parser "STRING_CONSTANT" string_or_arch_constant_p
int_arg_p = liftM IntConstantArg $ make_wrapper_parser "INT_CONSTANT" int_constant_p
stack_arg_p = liftM StackAddressArg $ make_wrapper_parser "STACK_ADDRESS" int_constant_p
char_arg_p = liftM CharConstantArg $ make_wrapper_parser "CHAR_CONSTANT" char_constant_p
identifier_arg_p = liftM IdentArg $ identifier_p

string_or_arch_constant_p = (liftM StringLit $ string_constant_p) <|> arch_constant_p

-- [Parsing of macro statements]

macro_statement_p :: CharParser () Statement
macro_statement_p = ws_p *> careful_choice [if_block_p, while_block_p, call_p] <* ws_p

if_block_p :: CharParser () Statement
if_block_p = make_macro_parser IfBlock if_block_head_p macro_statement_p

if_block_head_p = make_wrapper_parser "IF" identifier_p

while_block_p :: CharParser () Statement
while_block_p = make_macro_parser WhileBlock while_block_head_p macro_statement_p

while_block_head_p = make_wrapper_parser "WHILE" identifier_p

call_p :: CharParser () Statement
call_p = make_macro_parser CallKeyword keyword_identifier_p arg_p

-- [Parsing of macro definitions]

macro_def_p :: CharParser () Macro
macro_def_p = make_macro_parser MacroDef macro_head_p macro_statement_p

macro_head_p = make_wrapper_parser "DEF_MACRO" (sepEndBy identifier_p (ws_p >> char ',' >> ws_p))

-- [Parsing of macro programs]

-- n.b. we parse eof to force parsing of entire input
program_p = ws_p *> (sepEndBy macro_def_p ws_p) <* eof

-- [Top-level parse function definition]
parse_macro_program :: String -> Either ParseError [Macro]
parse_macro_program input = parse program_p "failure" input
