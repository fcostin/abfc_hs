module Abfc.MachineCodegen where

import Abfc.Machine
import Abfc.Allocator
import Abfc.Macros (LAddress(StackAddressConstant))



clear_ :: Int -> Machine -> (String, Machine)
clear_ dst = chain [
    move_to_stack_address dst,
    begin,
    dec 1,
    end]

clear dst = allocless $ clear_ dst



destructive_add_ :: Int -> Int -> Machine -> (String, Machine)
destructive_add_ src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address src,
    end]

destructive_add src dst = allocless $ destructive_add_ src dst



destructive_sub_ :: Int -> Int -> Machine -> (String, Machine)
destructive_sub_ src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    dec 1,
    move_to_stack_address src,
    end]

destructive_sub src dst = allocless $ destructive_sub_ src dst



move_ :: Int -> Int -> Machine -> (String, Machine)
move_ src dst = chain [
    clear_ dst,
    destructive_add_ src dst]

move src dst = allocless $ move_ src dst



begin_loop_ :: Int -> Machine -> (String, Machine)
begin_loop_ src = chain [
    move_to_stack_address src,
    begin]

begin_loop src = allocless $ begin_loop_ src



end_loop_ :: Int -> Machine -> (String, Machine)
end_loop_ src = chain [
    move_to_stack_address src,
    end]

end_loop src = allocless $ end_loop_ src



copy_noalloc_ src dst tmp = chain [
    clear_ dst,
    clear_ tmp,
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address tmp,
    inc 1,
    move_to_stack_address src,
    end,
    destructive_add_ tmp src]



copy :: Int -> Int -> Machine -> Allocator -> (String, Machine, Allocator)
copy src dst bf alloc = let
        StackAddressConstant tmp = next_free_cell alloc
        alloc' = mark_cell_used (StackAddressConstant tmp) alloc
        alloc'' = free_cell (StackAddressConstant tmp) alloc'
    in
        return_with_alloc (copy_noalloc_ src dst tmp bf) alloc''


-- some helper functions to marginly reduce boilerplate

return_with_alloc :: (String, Machine) -> Allocator -> (String, Machine, Allocator)
return_with_alloc (ops, bf) alloc = (ops, bf, alloc)

allocless :: (Machine -> (String, Machine)) -> Machine -> Allocator -> (String, Machine, Allocator)
allocless f bf alloc = return_with_alloc (f bf) alloc
