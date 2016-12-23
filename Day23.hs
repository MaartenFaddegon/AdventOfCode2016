import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap

data Val = V Int | R Int deriving (Show, Eq)
data Instr = CPY Val Val | INC Val | DEC Val | JNZ Val Val | TGL Val deriving Show
type Env = IntMap Int
type Prgm = [Instr]

set :: Int -> Int -> Env -> Env
set r v = IntMap.insert r v

get :: Int -> Env -> Int
get r env = env ! r

env0 :: Env
env0 = foldr (\r -> set r 0) IntMap.empty [0..3]

-- cpy x y copies x (either an integer or the value of a register) into register y.
-- inc x increases the value of register x by one.
-- dec x decreases the value of register x by one.
-- jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

cpy (V _)  (V _) env = (env, 1)
cpy (V v)  (R r) env = (set r v env, 1)
cpy (R r') (R r) env = (set r v env, 1)
  where v = get r' env

inc (V _) env = (env, 1)
inc (R r) env = (set r (v+1) env, 1)
  where v = get r env

dec (V _) env = (env, 1)
dec (R r) env = (set r (v-1) env, 1)
  where v = get r env

jnz (R r1) (R r2) env = jnz (V $ get r1 env) (V $ get r2 env) env
jnz (R r) (V v) env   = jnz (V $ get r env) (V v) env
jnz (V v) (R r) env   = jnz (V v) (V $ get r env) env
jnz (V c) (V v) env   = case c of 0 -> (env, 1)
                                  _ -> (env, v)

tgl pc (R r) env instr = tgl pc (V $ get r env) env instr
tgl pc (V v) env instr
  | outOfBounds  = (instr,                        (env, 1))
  | otherwise    = (substitute (pc+v) tgl' instr, (env, 1))
  where 
  outOfBounds = pc+v >= length instr || pc+v < 0  

tgl' (TGL x)   = INC x
tgl' (DEC x)   = INC x
tgl' (INC x)   = DEC x
tgl' (JNZ x y) = CPY x y
tgl' (CPY x y) = JNZ x y

substitute idx fn xs = ys ++ (fn x) : zs
  where (ys, x:zs) = splitAt idx xs

eval = eval' 0  (set 0 7 $ env0)
eval2 = eval' 0 (set 0 12 $ env0)

eval' :: Int -> Env -> Prgm -> [String]
eval' pc env instr
  | pc >= length instr = [showEnv env]
  | pc + 2 < length instr && isMul i0 i1 i2 = 
      let (env', d) = mul i0 i1 env
      in (show pc ++ ": mul " ++ showEnv env) : eval' (pc+d) env' instr
  | otherwise          = 
      let (instr', (env', d)) = case i of
                        CPY x y -> (instr, cpy x y env)
                        INC x   -> (instr, inc x   env)
                        DEC x   -> (instr, dec x   env)
                        JNZ x y -> (instr, jnz x y env)
                        TGL x   -> tgl pc x env instr
      in (show pc ++ ": " ++ show i ++ " " ++ showEnv env)
         : eval' (pc+d) env' instr'
  where i = instr !! pc
        i0 = i
        i1 = instr !! (pc + 1)
        i2 = instr !! (pc + 2)

isMul (INC (R r1)) (DEC (R r2)) (JNZ (R r2') (V (-2))) = r2 == r2'
isMul _ _ _ = False

mul (INC (R r1)) (DEC (R r2)) env = (set r1 (v1*v2) . set r2 0 $ env, 3)
  where v1 = get r1 env
        v2 = get r2 env

showEnv :: Env -> String
showEnv = show . map snd . IntMap.toList 

a = R 0
b = R 1
c = R 2
d = R 3

ex1 = eval 
  [ CPY (V 2) a
  , TGL a
  , TGL a
  , TGL a
  , CPY (V 1) a
  , DEC a
  , DEC a
  ]

solution1 = eval input

solution2 = eval2 input
main = mapM putStrLn solution2

input = 
  [ CPY a b
  , DEC b
  , CPY a d
  , CPY (V 0) a
  , CPY b c
  , INC a
  , DEC c
  , JNZ c (V (-2))
  , DEC d
  , JNZ d (V (-5))
  , DEC b
  , CPY b c
  , CPY c d
  , DEC d
  , INC c
  , JNZ d (V (-2))
  , TGL c
  , CPY (V (-16)) c
  , JNZ (V 1) c
  , CPY (V 73) c
  , JNZ (V 82) d
  , INC a
  , INC d
  , JNZ d (V (-2))
  , INC c
  , JNZ c (V (-5))
  ]

