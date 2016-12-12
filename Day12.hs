import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IntMap

data Val = V Int | R Int
data Instr = CPY Val Val | INC Val | DEC Val | JNZ Val Val
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

cpy (V v)  (R r) env = (set r v env, 1)
cpy (R r') (R r) env = (set r v env, 1)
  where v = get r' env

inc (R r) env = (set r (v+1) env, 1)
  where v = get r env

dec (R r) env = (set r (v-1) env, 1)
  where v = get r env

jnz (R r) (V v) env = jnz (V $ get r env) (V v) env
jnz (V c) (V v) env = case c of
  0 -> (env, 1)
  _ -> (env, v)

eval = eval' 0 env0

eval2 = eval' 0 (set 2 1 $ env0)

eval' :: Int -> Env -> Prgm -> Int
eval' pc env instr
  | pc >= length instr = get 0 env
  | otherwise          = 
      let (env', d) = case instr !! pc of
                        CPY x y -> cpy x y env
                        INC x   -> inc x   env
                        DEC x   -> dec x   env
                        JNZ x y -> jnz x y env
      in eval' (pc+d) env' instr

a = R 0
b = R 1
c = R 2
d = R 3

ex1 = print $
  eval [CPY (V 41) a
       ,INC a
       ,INC a
       ,DEC a
       ,JNZ a (V 2)
       ,DEC a
       ]

main = solution2
solution1 = print $ eval input
solution2 = print $ eval2 input

input =
  [ CPY (V 1) a
  , CPY (V 1) b
  , CPY (V 26) d
  , JNZ c (V 2)
  , JNZ (V 1) (V 5)
  , CPY (V 7) c
  , INC d
  , DEC c
  , JNZ c (V (-2))
  , CPY a c
  , INC a
  , DEC b
  , JNZ b (V (-2))
  , CPY c b
  , DEC d
  , JNZ d (V (-6))
  , CPY (V 16) c
  , CPY (V 17) d
  , INC a
  , DEC d
  , JNZ d (V (-2))
  , DEC c
  , JNZ c (V (-5))
  ]
