module Turing where

-- Simple representation of an Abstract Turing Machine

-- A tape is a list of symbols
type Tape a = [a]

type State a = a

type Operation a = a

-- A instruction is a "quadruple" of four elements:
-- The current state
-- Symbol being observed
-- The operation to execute
-- The resultant state after execute the operation
type Instruction a b s = ((State s, a), (Operation b, State s))

-- A configuration is formed by a tape, the index of a symbol in the tape, and a
-- initial state
type Config a s = (Tape a, Int, State s)

-- The complete machine structure
data Machine a b s = Machine { config       :: Config a s
                             -- An instruction table
                             , instructions :: [Instruction a b s]
                             -- A function to interpret operations
                             , runOperation :: Operation b -> Int -> Tape a -> (Tape a, Int)
                             -- An infinite list containing only empty cells
                             , blankTape    :: Tape a
                             }

-- Receives a machine as input, and returns the final configuration
run :: (Eq a, Eq s) => Machine a b s -> Config a s
run (Machine (tape, idx, st) ins runOp empty) =
  -- If the instruction doesn't exist, then the machine stops
  maybe (tape, idx, st) f instruction
  -- Otherwise, the instruction is interpreted and the next configuration
  -- is processed
  where f (op, st') = let (tape', idx') = runOp op idx tape
                      in run (Machine (tape', idx', st') ins runOp empty)
        -- Here, the tape is surrounded by empty elements, making it
        -- "potencially" infinite
        instruction = lookup (st, (e : tape ++ empty) !! (idx + 1)) ins
        (e:_) = empty
