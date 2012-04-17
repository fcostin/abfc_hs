module Abfc.MachineCodegen where

import qualified Abfc.Machine as Machine
import Abfc.Allocator
import Abfc.Macros (LAddress(StackAddressConstant))

-- define how to transform base machine actions into actions that also carry allocator state

type MachineAction = Machine.Machine -> (String, Machine.Machine)
type CodeGenAction = Machine.Machine -> Allocator -> (String, Machine.Machine, Allocator)

return_with_alloc :: (String, Machine.Machine) -> Allocator -> (String, Machine.Machine, Allocator)
return_with_alloc (ops, bf) alloc = (ops, bf, alloc)

allocless :: MachineAction -> CodeGenAction
allocless f bf alloc = return_with_alloc (f bf) alloc

-- define list of basic machine actions

move_to_stack_address addr = allocless $ Machine.move_to_stack_address addr
left = allocless Machine.left
right = allocless Machine.right
inc n = allocless $ Machine.inc n
dec n = allocless $ Machine.dec n
read = allocless Machine.read
write = allocless Machine.write
begin = allocless Machine.begin
end = allocless Machine.end

-- define list of basic allocator actions


-- allocate the cell at the given address
--      precondition: the given address must be a free cell!
--      to find the address of a free cell, use next_free_cell
--      on the Allocator in question...
allocate :: Int -> CodeGenAction
allocate addr bf alloc = let
        alloc' = mark_cell_used (StackAddressConstant addr) alloc
    in
        ([], bf, alloc)

-- deallocate the cell at the given address
free :: Int -> CodeGenAction
free addr bf alloc = let
        alloc' = free_cell (StackAddressConstant addr) alloc
    in
        ([], bf, alloc')

-- define how to compose sequences of machine-allocator actions

(>>) :: CodeGenAction -> CodeGenAction -> CodeGenAction
(>>) f g bf alloc = let
        (ops', bf', alloc') = f bf alloc
        (ops'', bf'', alloc'') = g bf' alloc'
    in
        (ops' ++ ops'', bf'', alloc'') -- nb note concat order!

id :: CodeGenAction
id bf alloc = ("", bf, alloc)

chain :: [CodeGenAction] -> CodeGenAction
chain actions = foldl (Abfc.MachineCodegen.>>) (Abfc.MachineCodegen.id) actions

-- adapt a three argument action to use an allocated argument for the third argument
-- n.b. the allocated cell is automatically freed
allocate_third :: (Int -> Int -> Int -> CodeGenAction) -> Int -> Int -> CodeGenAction
allocate_third f x y bf alloc = let
        StackAddressConstant tmp = next_free_cell alloc
    in
        chain [allocate tmp, f x y tmp, free tmp] bf alloc

-- adapt a four argument action to use an allocated argument for the fourth argument
-- n.b. the allocated cell is automatically freed
allocate_fourth :: (Int -> Int -> Int -> Int -> CodeGenAction) -> Int -> Int -> Int -> CodeGenAction
allocate_fourth f x y z bf alloc = let
        StackAddressConstant tmp = next_free_cell alloc
    in
        chain [allocate tmp, f x y z tmp, free tmp] bf alloc
 


 



-- now, we define the built-in macros:


clear :: Int -> CodeGenAction
clear dst = chain [
    move_to_stack_address dst,
    begin,
    dec 1,
    end]



destructive_add :: Int -> Int -> CodeGenAction
destructive_add src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address src,
    end]



destructive_sub :: Int -> Int -> CodeGenAction
destructive_sub src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    dec 1,
    move_to_stack_address src,
    end]



move :: Int -> Int -> CodeGenAction
move src dst = chain [
    clear dst,
    destructive_add src dst]



begin_loop :: Int -> CodeGenAction
begin_loop src = chain [
    move_to_stack_address src,
    begin]



end_loop :: Int -> CodeGenAction
end_loop src = chain [
    move_to_stack_address src,
    end]



copy :: Int -> Int -> CodeGenAction
copy = allocate_third copy_noalloc

copy_noalloc :: Int -> Int -> Int -> CodeGenAction
copy_noalloc src dst tmp = chain [
    clear dst,
    clear tmp,
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address tmp,
    inc 1,
    move_to_stack_address src,
    end,
    destructive_add tmp src]



stack_add :: Int -> Int -> CodeGenAction
stack_add = allocate_third stack_add_noalloc

stack_add_noalloc :: Int -> Int -> Int -> CodeGenAction
stack_add_noalloc src dst tmp = chain [
    copy src tmp,
    destructive_add tmp dst]



constant_add :: Int -> Int -> CodeGenAction
constant_add n dst = chain [
    move_to_stack_address dst,
    inc n]



stack_sub :: Int -> Int -> CodeGenAction
stack_sub = allocate_third stack_sub_noalloc

stack_sub_noalloc :: Int -> Int -> Int -> CodeGenAction
stack_sub_noalloc src dst tmp = chain [
    copy src tmp,
    destructive_sub tmp dst]



constant_sub :: Int -> Int -> CodeGenAction
constant_sub n dst = chain [
    move_to_stack_address dst,
    dec n]



as_logical :: Int -> Int -> CodeGenAction
as_logical = allocate_third as_logical_noalloc

as_logical_noalloc :: Int -> Int -> Int -> CodeGenAction
as_logical_noalloc src dst tmp = chain [
    copy src tmp,
    clear dst,
    move_to_stack_address tmp,
    begin,
    move_to_stack_address dst,
    inc 1,
    clear tmp,
    end]



logical_not :: Int -> Int -> CodeGenAction
logical_not = allocate_third logical_not_noalloc

logical_not_noalloc :: Int -> Int -> Int -> CodeGenAction
logical_not_noalloc src dst tmp = chain [
    copy src tmp,
    clear dst,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address tmp,
    begin,
    move_to_stack_address dst,
    dec 1,
    clear tmp,
    end]



logical_or :: Int -> Int -> Int -> CodeGenAction
logical_or = allocate_fourth logical_or_noalloc

logical_or_noalloc :: Int -> Int -> Int -> Int -> CodeGenAction
logical_or_noalloc src_a src_b dst tmp = chain [
    clear dst,
    as_logical src_a tmp,
    stack_add tmp dst,
    as_logical src_b tmp,
    stack_add tmp dst,
    as_logical dst tmp,
    move tmp dst]



logical_and :: Int -> Int -> Int -> CodeGenAction
logical_and = allocate_fourth logical_or_noalloc

logical_and_noalloc :: Int -> Int -> Int -> Int -> CodeGenAction
logical_and_noalloc src_a src_b dst tmp = chain [
    clear dst,
    logical_not src_a tmp,
    stack_add tmp dst,
    logical_not src_b tmp,
    stack_add tmp dst,
    logical_not dst tmp,
    move tmp dst]



get_char :: Int -> CodeGenAction
get_char dst = chain [
    move_to_stack_address dst,
    Abfc.MachineCodegen.read]



put_char :: Int -> CodeGenAction
put_char src = chain [
    move_to_stack_address src,
    write]


{- TODO
put_string_constant :: String -> CodeGenAction
put_string_constant s bf alloc = let
        StackAddressConstant tmp = next_free_cell alloc
    in
        chain [allocate tmp, clear tmp, free tmp] bf alloc

-}
