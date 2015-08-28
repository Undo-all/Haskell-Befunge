import Control.Monad.State
import System.Random (randomRIO)
import qualified Data.Vector as V
import Data.Char (isDigit, chr, ord)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data Direction = U
               | D
               | L
               | R deriving (Eq, Show)

data InterpreterState = InterpreterState
                      { stringMode :: Bool
                      , running :: Bool
                      , stack :: [Int]
                      , direction :: Direction
                      , insPtr :: (Int, Int)
                      , program :: V.Vector (V.Vector Char)
                      } deriving (Eq, Show)

type Interpreter = StateT InterpreterState IO 

top :: Interpreter Int
top = do
    stk <- gets stack
    case stk of
      [] -> return 0
      _  -> return (head stk)

pop :: Interpreter Int
pop = do
    stk <- gets stack
    case stk of
      [] -> return 0
      _  -> do modify $ \s -> s { stack = tail (stack s) }
               return (head stk)

push :: Int -> Interpreter ()
push n = do
    modify $ \s -> s { stack = n : (stack s) }

changeDir :: Direction -> Interpreter ()
changeDir d = modify $ \s -> s { direction = d }

move :: Interpreter ()
move = do
    dir <- gets direction
    (x, y) <- gets insPtr
    prog <- gets program
    let width  = V.length (prog V.! y)
        height = V.length prog
        newPtr = 
          case dir of 
            U -> (x, wrap (y - 1) height)
            D -> (x, wrap (y + 1) height)
            L -> (wrap (x - 1) width, y)
            R -> (wrap (x + 1) width, y)
    modify $ \s -> s { insPtr = newPtr }
    where wrap x y
              | x >= y    = 0
              | x < 0     = y - 1
              | otherwise = x

interpret :: Char -> Interpreter ()
interpret '+' = do
    a <- pop
    b <- pop
    push (a+b)
interpret '-' = do
    a <- pop
    b <- pop
    push (b-a)
interpret '*' = do
    a <- pop
    b <- pop
    push (a*b)
interpret '/' = do
    a <- pop
    b <- pop
    case a of 
      0 -> do lift $ putStr "Division by zero! Desired value: "
              lift $ hFlush stdout
              n <- lift getLine
              push $ read n
      _ -> push $ floor ((fromIntegral b) / (fromIntegral a))
interpret '%' = do
    a <- pop
    b <- pop
    push $ a `mod` a
interpret '!' = do
    a <- pop
    if a == 0 then push 1 else push 0
interpret '`' = do
    a <- pop
    b <- pop
    if b > a then push 1 else push 0
interpret '>' = changeDir R
interpret '<' = changeDir L
interpret '^' = changeDir U
interpret 'v' = changeDir D
interpret '?' = do
    newDir <- lift $ choice [U,D,L,R]
    changeDir newDir
    where choice xs = (xs !!) <$> randomRIO (0, length xs - 1) 
interpret '_' = do
    a <- pop
    changeDir $ if a == 0 then R else L  
interpret '|' = do
    a <- pop
    changeDir $ if a == 0 then D else U
interpret '"' = do
    modify $ \s -> s { stringMode = not (stringMode s) }
interpret ':' = do
    a <- top
    push a
interpret '\\' = do
    a <- pop
    b <- pop
    push a
    push b
interpret '$' = do
    pop
    return ()
interpret '.' = do
    a <- pop
    lift $ putStr (show a)
interpret ',' = do
    a <- pop
    lift $ putChar (chr a)
interpret '#' = move
interpret 'g' = do
    y <- pop
    x <- pop
    prog <- gets program
    push $ ord (prog V.! y V.! x)
interpret 'p' = do
    y <- pop
    x <- pop
    v <- pop
    modify $ \s -> s { program = program s V.// [(y, (program s V.! y) V.// [(x, chr v)])] }
interpret '&' = do
    x <- lift $ getLine
    push $ read x
interpret '~' = do
    x <- lift $ getChar
    push $ ord x
interpret '@' = do
    modify $ \s -> s { running = False } 
interpret c | isDigit c = push $ ord c - 48
interpret _ = return ()

eval :: Interpreter ()
eval = do 
    r <- gets running
    when r $
      do (x, y) <- gets insPtr
         prog <- gets program
         let c = prog V.! y V.! x
         sm <- gets stringMode
         if sm 
           then case c of 
                   '"' -> modify $ \s -> s { stringMode = not (stringMode s) }
                   _   -> push (ord c)
           else interpret c
         move
         eval

initState prog = InterpreterState False True [] R (0, 0) prog

run :: String -> IO ()
run xs = evalStateT eval (initState prog)
    where prog       = V.fromList $ map V.fromList (pad $ lines xs)
          pad xss    = map (space (maximum (map length xss))) xss
          space n xs = xs ++ replicate (n - length xs) ' '

main = do
    file <- head <$> getArgs
    code <- readFile file
    run code
