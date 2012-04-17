import Abfc.Parser (parse_macro_program)
import Abfc.Convert (convert_program)
import Abfc.Compile (compile_macro_program_alpha, compile_macro_program_beta)

import Abfc.ParserMacros (Macro)
import Abfc.Macros (LMacro, LStatement)

import Abfc.Eval (evaluate)

chunk_string :: Int -> String -> [String] -> [String]
chunk_string _ [] acc = acc
chunk_string n s acc = let
        chunk = take n s
        m = length chunk
        chunk' = if m == n then chunk else chunk ++ (replicate (n - m) ' ')
    in
        chunk_string n (drop n s) (acc ++ [chunk'])

pad_zip :: a -> a -> [a] -> [a] -> [(a, a)]
pad_zip cx cy xs ys = let
        m = length xs
        n = length ys
        o = max m n
        xs' = if o == m then xs else xs ++ (replicate (o - m) cx)
        ys' = if o == n then ys else ys ++ (replicate (o - n) cy)
    in
        zip xs' ys'

two_col_layout :: Int -> Int -> Int -> String -> String -> [String]
two_col_layout l m r ls rs = let
        ls' = chunk_string l ls []
        rs' = chunk_string r rs []
        lblank = (replicate l ' ')
        mblank = (replicate m ' ')
        rblank = (replicate r ' ')
        lrs = pad_zip lblank rblank ls' rs'
        f (x, y) = x ++ mblank ++ y ++ "\n"
    in
        map f lrs

format_annotated_code code = let
        l_width = 40
        m_width = 10
        r_width = 40
        f (a, b) = concat $ two_col_layout l_width m_width r_width a (show b)
    in
        map f code

do_compile parsed_program = do
    let macros = convert_program parsed_program
    let macros' = compile_macro_program_alpha macros
    let main_body = compile_macro_program_beta macros'
    let code = evaluate main_body
    mapM_ putStrLn (format_annotated_code code)

-- [main entry point]
main = do
    macro_source <- getContents
    case parse_macro_program macro_source of
        Left e -> putStrLn ("parse error: " ++ (show e))
        Right x -> do_compile x
    putStrLn ""
