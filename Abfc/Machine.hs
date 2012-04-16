module Abfc.Machine (
    Machine,
    initial,
    move_to_stack_address,
    left,
    right,
    inc,
    dec,
    Abfc.Machine.read,
    write,
    begin,
    end,
    (Abfc.Machine.>>),
    chain) where


-- Bf buffer_size data_pointer stack_pointer loop_stack
data Machine = Bf Int Int Int [Int]

initial :: Machine
initial = (Bf 30000 0 0 [])


validated :: Machine -> Machine
validated (Bf n dp sp stack) | (0 <= dp) && (dp < n) = (Bf n dp sp stack)
validated _ = error "Machine Error: data pointer out of bounds"


move :: Int -> Machine -> ([Char], Machine)
move offset bf | (offset == 0) = ([], validated bf)
move offset (Bf n dp sp stack) | (offset > 0) = let
        ops = replicate offset '>'
        dp' = dp + offset
    in
        (ops, validated (Bf n dp' sp stack))
move offset (Bf n dp sp stack) | (offset < 0) = let
        ops = replicate (-offset) '<'
        dp' = dp + offset
    in
        (ops, validated (Bf n dp' sp stack))


move_to_stack_address :: Int -> Machine -> ([Char], Machine)
move_to_stack_address stack_addr (Bf n dp sp stack) = let
        dest = sp + stack_addr
        offset = dest - dp
    in
        move offset (Bf n dp sp stack)


left :: Machine -> ([Char], Machine)
left bf = move (-1) bf


right :: Machine -> ([Char], Machine)
right bf = move 1 bf


inc :: Int -> Machine -> ([Char], Machine)
inc x bf | (x >= 0) = ((replicate x '+'), bf)
inc x bf = error "Machine Error: refusing to inc by negative"


dec :: Int -> Machine -> ([Char], Machine)
dec x bf | (x >= 0) = ((replicate x '-'), bf)
dec x bf = error "Machine Error: refusing to dec by negative"


read :: Machine -> ([Char], Machine)
read bf = (",", bf)


write :: Machine -> ([Char], Machine)
write bf = (".", bf)


begin :: Machine -> ([Char], Machine)
begin (Bf n dp sp stack) = ("[", validated (Bf n dp sp ((dp - sp):stack)))


end :: Machine -> ([Char], Machine)
end (Bf n dp sp stack) =
    case stack of
        (x:xs) | (x == (dp - sp)) -> ("]", (Bf n dp sp xs))
        (x:xs) -> error "Machine Error: divergent dp values at end loop"
        _ -> error "Machine Error: end loop without begin loop"


type MachineAction = Machine -> ([Char], Machine)
(>>) :: MachineAction -> MachineAction -> MachineAction
(>>) f g bf = let
        (ops', bf') = f bf
        (ops'', bf'') = g bf'
    in
        (ops'' ++ ops', bf'')

id :: MachineAction
id bf = ([], bf)

chain :: [MachineAction] -> MachineAction
chain actions = foldl (Abfc.Machine.>>) (Abfc.Machine.id) actions
