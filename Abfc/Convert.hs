module Abfc.Convert where

import Abfc.ParserMacros
import Abfc.Macros

convert_program = map convert_macro

convert_macro :: Macro -> LMacro
convert_macro m =
    case m of
        Abfc.ParserMacros.MacroDef ((Abfc.ParserMacros.Ident name):params) statements -> let
                params' = map convert_ident params
                statements' = map convert_statement statements
            in
                Abfc.Macros.MacroDef name params' statements'

convert_ident :: Identifier -> LIdentifier
convert_ident x =
    case x of 
        Abfc.ParserMacros.Ident s -> Abfc.Macros.Ident s

convert_statement :: Statement -> LStatement
convert_statement s =
    case s of
        Abfc.ParserMacros.IfBlock x statements -> let
                x' = convert_ident x
                statements' = map convert_statement statements
            in
                Abfc.Macros.IfBlock x' statements'
        Abfc.ParserMacros.WhileBlock x statements -> let
                x' = convert_ident x
                statements' = map convert_statement statements
            in
                Abfc.Macros.WhileBlock x' statements'
        CallKeyword (Keyword "CALL") ((Abfc.ParserMacros.IdentArg (Abfc.ParserMacros.Ident s)):args) -> let
                args' = map convert_arg args
            in
                UserMacroCall s args'
        CallKeyword (Keyword s) args -> let
                args' = map convert_arg args
            in
                BuiltInCall s args'

convert_arg :: Argument -> LArgument
convert_arg a =
    case a of
        Abfc.ParserMacros.IdentArg x -> Abfc.Macros.IdentArg (convert_ident x)
        StringConstantArg (StringLit s) -> StringConstant s
        StringConstantArg (ArchStringLit s) -> ArchStringConstant s
        IntConstantArg x -> IntConstant x
        StackAddressArg x -> StackAddressConstant x
        CharConstantArg c -> CharConstant c

