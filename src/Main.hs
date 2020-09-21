module Main where

import Turing

-- Example of a machine which doubles the 'I's in a tape

-- The possible symbols that a tape can have
data MySymbols = O -- An empty cell
               | I -- A filled cell
               deriving (Eq, Show)

-- Operations of the machine
data MyOps = R -- Move one cell to the right
           | L -- Move one cell to the left
           | E -- Empty the cell content
           | F -- Fill the cell

-- A string is used to identify the state
type MyState = String

-- The tape to process
myTape :: Tape MySymbols
myTape = [I, I, I, I]

-- The initial configuration
myCfg :: Config MySymbols String
myCfg = (myTape, 0, "s1")

-- The instruction table
myIns :: [Instruction MySymbols MyOps String]
myIns =  [ (("s1", I), (E, "s2"))
         , (("s2", O), (R, "s3"))
         , (("s3", I), (R, "s3"))
         , (("s3", O), (R, "s4"))
         , (("s4", O), (E, "s5"))
         , (("s4", I), (R, "s4"))
         , (("s5", O), (F, "s5"))
         , (("s5", I), (R, "s6"))
         , (("s6", O), (F, "s7"))
         , (("s7", I), (L, "s7"))
         , (("s7", O), (L, "s8"))
         , (("s8", I), (L, "s8"))
         , (("s8", O), (R, "s9"))
         , (("s9", I), (F, "s1"))
         , (("s9", O), (R, "s10"))
         ]

-- A function to interpret the operations
myRunOp :: MyOps -> Int -> Tape MySymbols -> (Tape MySymbols, Int)
myRunOp op idx tape =
  case op of
    R -> (tape, idx + 1)
    L -> (tape, idx - 1)
    F -> (l ++ I : r, idx)
    E -> (l ++ O : r, idx)
  where (l, r) = (take idx tape, drop (idx + 1) tape)

myMachine :: Machine MySymbols MyOps MyState
myMachine = Machine myCfg myIns myRunOp (repeat O)

main :: IO ()
main = do
  putStrLn $ "Initial tape: " ++ show myTape
  let (tape, _, _) = run myMachine
  putStrLn $ "Processed tape: " ++ show tape
