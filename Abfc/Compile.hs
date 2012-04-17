module Abfc.Compile where

import Abfc.Macros

compile_macro_program_alpha :: [LMacro] -> [LMacro]
compile_macro_program_alpha macros =
    map macro_rewrite_rule_alpha macros

compile_macro_program_beta :: [LMacro] -> [LStatement]
compile_macro_program_beta macros = let
        -- rewrite the body of the main user macro
        (MacroDef _ _ main_body) = unique_macro_by_name "main" macros
        main_body' =apply_until_fixed (rewrite_rule_beta macros) main_body
        header = [EnvBegin]
        footer = [EnvEnd]
    in
        header ++ main_body' ++ footer

macro_rewrite_rule_alpha (MacroDef s params body) = let
        body' = apply_until_fixed rewrite_rule_alpha body
    in
        MacroDef s params body'

-- [rewrite rule alpha : local, if, while expansion]

-- compose all grafted expansions in some order
-- n.b. each grafted expansion has type [LStatement] -> [LStatement]
rewrite_rule_alpha :: [LStatement] -> [LStatement]
rewrite_rule_alpha = foldl (.) id (map graft alpha_expansions)

alpha_expansions = [expand_local, expand_while_block, expand_if_block]

local :: LIdentifier -> LStatement
local x = BuiltInCall "LOCAL" [IdentArg x]

copy :: LIdentifier -> LIdentifier -> LStatement
copy src dst = BuiltInCall "COPY" [(IdentArg src), (IdentArg dst)]

clear :: LIdentifier -> LStatement
clear x = BuiltInCall "CLEAR" [IdentArg x]

begin_loop :: LIdentifier -> LStatement
begin_loop x = BuiltInCall "BEGIN_LOOP" [IdentArg x]

end_loop :: LIdentifier -> LStatement
end_loop x = BuiltInCall "END_LOOP" [IdentArg x]


expand_local :: LStatement -> [LStatement]
expand_local s =
    case s of
        BuiltInCall "LOCAL" [IdentArg x] ->
            [(EnvDeclare x), (EnvSet x AllocateLocal), (FreeLocalOnEnvExit x)]
        _ -> [s]

expand_while_block :: LStatement -> [LStatement]
expand_while_block s =
    case s of
        WhileBlock x statements ->
            [EnvBegin, (begin_loop x) ] ++ statements ++ [(end_loop x), EnvEnd]
        _ -> [s]

expand_if_block :: LStatement -> [LStatement]
expand_if_block s =
    case s of
        IfBlock x statements -> let
                t = HiddenIdent "if_temp"
                header = [EnvBegin, (local t), (copy x t), (begin_loop t)]
                footer = [(clear t), (end_loop t), EnvEnd]
            in
                header ++ statements ++ footer
        _ -> [s]

graft :: (a -> [a]) -> ([a] -> [a])
graft f xs = concat (map f xs)

-- repeatedly apply a function until we hit a fixed point
apply_until_fixed :: Eq a => (a -> a) -> a -> a
apply_until_fixed f x = let
        x' = f x
    in
        if (x' == x) then x else apply_until_fixed f x'

-- [rewrite rule beta : user macro expansion]

macro_name :: LMacro -> String
macro_name (MacroDef s _ _) = s

unique_macro_by_name :: String -> [LMacro] -> LMacro
unique_macro_by_name s ms = let
        shortlist = filter (\m -> (macro_name m == s)) ms
    in
        case shortlist of
            [m] -> m -- there can only be one

-- produce list of statements binding local parameters to arguments
bind :: (LArgument, LIdentifier) -> [LStatement]
bind (arg, param) =
    case arg of
        IdentArg x -> [(EnvDeclare param), (EnvSet param (OuterEnvGet x))]
        Address x -> [(EnvDeclare param), (EnvSet param x)]
        Constant c -> [(EnvDeclare param), (EnvSetConstant param c)]

zip_matching_length :: [a] -> [b] -> [(a, b)]
zip_matching_length as bs =
    case (length as, length bs) of
        (m, n) | (m == n) -> zip as bs
        _ -> error "error: zip_matching_length length mismatch"

inline_macro_body :: LMacro -> [LArgument] -> [LStatement]
inline_macro_body (MacroDef _ params body) args = let
        bindings = concat (map bind (zip_matching_length args params))
        header = [EnvBegin] ++ bindings
        footer = [EnvEnd]
    in
        header ++ body ++ footer

expand_call_site :: [LMacro] -> LStatement -> [LStatement]
expand_call_site macros s =
    case s of
        UserMacroCall macro_name args -> let
                m = unique_macro_by_name macro_name macros
            in
                inline_macro_body m args 
        _ -> [s]

rewrite_rule_beta :: [LMacro] -> [LStatement] -> [LStatement]
rewrite_rule_beta macros body = 
    graft (expand_call_site macros) body
