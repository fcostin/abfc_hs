import Abfc.Parser (parse_macro_program)
import Abfc.Convert (convert_program)
import Abfc.Compile (compile_macro_program_alpha, compile_macro_program_beta)

import Abfc.ParserMacros (Macro)
import Abfc.Macros (LMacro, LStatement)

import Abfc.Eval (evaluate)



print_list body = do
    mapM_ putStrLn (map show body)

do_compile parsed_program = do
    let macros = convert_program parsed_program
    let macros' = compile_macro_program_alpha macros
    let main_body = compile_macro_program_beta macros'
    let code = evaluate main_body
    putStrLn ">>> parsed macros:"
    putStrLn (show parsed_program)
    putStrLn ">>> converted macros:"
    putStrLn (show macros)
    putStrLn ">>> phase alpha output:"
    putStrLn (show macros')
    putStrLn ">>> phase beta output:"
    print_list main_body
    putStrLn ">>> evaluation output:"
    print_list code 

-- [main entry point]
main = do
    macro_source <- getContents
    case parse_macro_program macro_source of
        Left e -> putStrLn ("parse error: " ++ (show e))
        Right x -> do_compile x
    putStrLn "done."
