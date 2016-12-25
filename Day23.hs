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
  | pc + 6 < length instr && isMul env i0 i1 i2 i3 i4 i5 = 
      let env' :: Env 
          env' = mul i0 i1 i4 env
      in (show pc ++ ": " ++ showMul i0 i1 i4 ++ showEnv env) : eval' (pc+6) env' instr
  | pc + 2 < length instr && isAdd env i0 i1 i2 = 
      let (env', d) = add i0 i1 env
      in (show pc ++ ": " ++ showAdd i0 i1 ++ showEnv env) : eval' (pc+d) env' instr
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
        i3 = instr !! (pc + 3)
        i4 = instr !! (pc + 4)
        i5 = instr !! (pc + 5)

-- isAdd env (INC (R r1)) 
--           (DEC (R r2)) 
--           (JNZ (R r2') (V (-2))) = 
--   r2 == r2' && (get r1 env) /= 0 && (get r2 env) > 0
isAdd _ _ _ _ = False

add (INC (R r1)) (DEC (R r2)) env = (set r1 (v1+v2) . set r2 0 $ env, 3)
  where v1 = get r1 env
        v2 = get r2 env

showAdd (INC r1) (DEC r2) = "add (" ++ show r1 ++ ") (" ++ show r2 ++ ") "

-- i0: CPY (R 1) (R 2)      r2 := r1
-- i1: INC (R 0)            do { r0++
-- i2: DEC (R 2)                 r2--
-- i3: JNZ (R 2) (V (-2))   while } (r2>0)
-- i4: DEC (R 3)            r3--
-- i5: JNZ (R 3) (V (-5))   while (r3>0)
-- 
-- when r1 > 0 && r3 > 0
-- r0 := r0 + r1 * r3
-- r2 := 0
-- r3 := 0

isMul env (CPY (R r1) (R r2))
          (INC (R r0)) 
          (DEC (R r2')) 
          (JNZ (R r2'') (V (-2)))
          (DEC (R r3))
          (JNZ (R r3') (V (-5))) =
  r2 == r2' && r2 == r2'' && r3 == r3' && (get r1 env) > 0 && (get r3 env) > 0
isMul _ _ _ _ _ _ _ = False

mul :: Instr -> Instr -> Instr -> Env -> Env
mul (CPY (R r1) (R r2)) (INC (R r0)) (DEC (R r3)) env =
  (set r0 v) . (set r2 0) . (set r3 0) $ env
   where v = (get r0 env) + ((get r1 env) * (get r3 env))

showMul (CPY (R r1) (R r2)) (INC (R r0)) (DEC (R r3)) =
  "r" ++ show r0 ++ " += r" ++ show r2 ++ " * r" ++ show r3


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

