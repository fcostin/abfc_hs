module Abfc.MachineCodegen where

import Abfc.Machine

clear :: Int -> Machine -> (String, Machine)
clear dst = chain [
    move_to_stack_address dst,
    begin,
    dec 1,
    end]

destructive_add :: Int -> Int -> Machine -> (String, Machine)
destructive_add src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    inc 1,
    move_to_stack_address src,
    end]

destructive_sub :: Int -> Int -> Machine -> (String, Machine)
destructive_sub src dst = chain [
    move_to_stack_address src,
    begin,
    dec 1,
    move_to_stack_address dst,
    dec 1,
    move_to_stack_address src,
    end]

move :: Int -> Int -> Machine -> (String, Machine)
move src dst = chain [
    clear dst,
    destructive_add src dst]

begin_loop :: Int -> Machine -> (String, Machine)
begin_loop src = chain [
    move_to_stack_address src,
    begin]

end_loop :: Int -> Machine -> (String, Machine)
end_loop src = chain [
    move_to_stack_address src,
    end]
