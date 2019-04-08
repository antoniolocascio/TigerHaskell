{-# Language OverloadedStrings #-}
import           Tools
import           TigerTree
import           TigerInterpFrame
import           TigerInteractive
import           TigerInterpTypes
import           TigerSymbol
import           TigerTemp
import           Control.Conditional
import           Data.Map                      as M

procs = []
strings = [(pack "str1", pack "hola")]
mainStms = [res suma]
frame = defaultFrame

main :: IO ()
main = do
  putStrLn "\n======= Test Interpreter n progress ======="
  ifM (testinterp procs strings (mainStms, frame) 29) bluenice redfail

  --startInterp procs strings (mainStms, frame)
  putStrLn "\n======= Test FIN ======="

testinterp
  :: [(Frame, [Stm])] -> [(Label, Symbol)] -> ([Stm], Frame) -> Int -> IO Bool
testinterp pr strs main_ r = do
  (res, _) <- startInterp pr strs main_
  return $ res == r


suma :: Exp
suma = Binop Plus (Const 14) (Const 15)

sumareg :: Exp
sumareg = Binop Plus (Temp (pack "r1")) (Temp (pack "r2"))

res :: Exp -> Stm
res = Move (Temp rv)

callP = Call (Name "print") [Name "str1"]
-- print("hola")
