import Abfc.Parser (parse_macro_program)

-- [main entry point]
main = do
    macro_source <- getContents
    case parse_macro_program macro_source of
        Left e -> putStrLn ("parse error: " ++ (show e))
        Right x -> putStrLn (show x)
    putStrLn "done."
