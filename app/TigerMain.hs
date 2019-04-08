module Main
  ( main
  )
where
import           Control.Monad
import           Control.Monad.State     hiding ( evalState )
import qualified Control.Monad.State           as ST
                                                ( evalState )
import           Data.Either
import           Data.Maybe
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit

import           TigerAbs
import           TigerEscap
import           TigerParser
import           TigerPrettyRaw
import           TigerSeman
import           TigerTemp
import           TigerUnique
import           TigerSymbol
import           TigerTrans
import           TigerTips
import           TigerPrettyIr
import           TigerFrame
import qualified TigerInterpFrame              as IF
import qualified TigerTree                     as Tree
import           TigerCanon
import           Text.Parsec                    ( runParser )
import           System.IO
import           TigerInterpPP
import           TigerInterpTypes
import qualified TigerInteractive              as Interactive
import           TigerConsFoldIr
import           TigerAssem
import           TigerAssemTypes
import           TigerFCG
import qualified Data.List                     as L
import           TigerSA
import           TigerInterf
import           Debug.Trace
import           TigerProcTransf

data Options = Options {
        optArbol     :: Bool
        ,optDebEscap :: Bool
        ,optIR       :: Bool
        ,optInterp   :: Bool
        ,optDbg      :: Bool
        ,optAssem :: Bool
        ,optM :: Bool
    }
    deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optArbol    = False
  , optDebEscap = False
  , optIR       = False
  , optInterp   = False
  , optDbg      = False
  , optAssem    = False
  , optM        = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['a']
           ["arbol"]
           (NoArg (\opts -> opts { optArbol = True }))
           "Muestra el AST luego de haber realizado el cálculo de escapes"
  , Option ['e']
           ["escapada"]
           (NoArg (\opts -> opts { optDebEscap = True }))
           "Stepper escapadas"
  , Option ['b']
           ["bexp"]
           (NoArg (\opts -> opts { optIR = True }))
           "Muestra el codigo intermedio"
  , Option ['i']
           ["interp"]
           (NoArg (\opts -> opts { optInterp = True }))
           "Ejecuta el interprete"
  , Option ['d']
           ["debugger"]
           (NoArg (\opts -> opts { optDbg = True }))
           "Ejecuta el debugger"
  , Option ['s']
           ["assembler"]
           (NoArg (\opts -> opts { optAssem = True }))
           "Muestra codigo assembler"
  , Option ['m']
           ["mips"]
           (NoArg (\opts -> opts { optM = True }))
           "Guardar archivo assembler a TigerHaskell/mips/"
  ]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Se usa: tiger fileName [OPTIONS] "

showExp :: Exp -> IO ()
showExp e = do
  putStrLn "Mostramos el AST"
  putStrLn $ renderExpRaw e

calculoEscapadas :: Exp -> Options -> IO Exp
calculoEscapadas rawAST opts = either
  (\err -> putStrLn "Error de Escap:" >> fail (show err))
  return
  (calcularEEsc rawAST)

canonize :: Tree.Stm -> TigerUnique.StGen [Tree.Stm]
canonize stm = do
  let stm' = constFoldS stm
  lin <- linearize stm'
  lss <- basicBlocks lin
  cod <- evalStateT (traceSchedule lss) firstTank
  flattenCalls cod

templabRel :: Exp -> StGen (Either Symbol ([Frag], [([Tree.Stm], Frame)]))
templabRel ast = do
  fM <- runSemanFrags ast
  either
    (return . Left)
    (\frags -> do
      let (strs, procs) = sepFrag frags
      x <- mapM (\(s, f) -> canonize s >>= \stms -> return (stms, f)) procs
      return $ Right (strs, x)
    )
    fM

parserStep :: Options -> String -> String -> IO Exp
parserStep opts nm sc =
  either (\perr -> error $ "Parser error" ++ show perr) return
    $ runParser expression () nm sc

runInterp :: Bool -> ([Frag], [([Tree.Stm], IF.Frame)]) -> IO ()
runInterp inter (strs_, blocks) =
  let (mainBlock, otherBlocks_) = sepMain blocks
      otherBlocks               = map (\(x, y) -> (y, x)) otherBlocks_
      strs                      = getStrs strs_                                                                                                                                                                                                                                                                                                                                                                                         --cpu = loadCPU [] [] [Tree.Move (Tree.Temp (pack "rv") (Tree.Const 1)]
  in  if inter
        then void $ Interactive.startInteractive otherBlocks strs mainBlock
        else void $ Interactive.startInterp otherBlocks strs mainBlock
 where
  sepMain
    :: [([Tree.Stm], IF.Frame)]
    -> (([Tree.Stm], IF.Frame), [([Tree.Stm], IF.Frame)])
  sepMain xs = (head xs, tail xs)
  getStrs :: [Frag] -> [(Label, Symbol)]
  getStrs = map (\(AString l sms) -> (l, Prelude.last sms))

getIns :: [[Tree.Stm]] -> Integer -> ([[Instr]], Integer)
getIns [] i = ([], i)
getIns (pr : procs) i =
  let (instrs, i' ) = evalState (runMunch pr) i
      (rest  , i'') = getIns procs i'
  in  (instrs : rest, i'')

makeCode :: [Frag] -> [([Tree.Stm], Frame)] -> Integer -> [String]
makeCode strs procs i =
  let
    bds         = fst <$> procs
    frames      = snd <$> procs
    instrs      = fst $ getIns bds i
    instrsf     = zipWith (\f bd -> (procEntryExit2 f bd, f)) frames instrs
    transformed = map (uncurry runTransformsASM) instrsf
    final = map (\(f, bd) -> procEntryExit3 f (getTempsL bd) bd) transformed
    strs_s      = map renderStrFrag strs
    assems_s    = map renderInstrs final
  in
    strs_s ++ assems_s

writeCode :: String -> Bool -> [String] -> IO ()
writeCode filename m strs =
  let toWrite = L.intercalate "\n" strs
      filepath =
        if m then "mips/" ++ getName filename ++ ".s" else filename ++ ".s"
  in  writeFile filepath toWrite
 where
  getName s = L.reverse $ getName' (L.reverse s)
  getName' []           = []
  getName' ('/' : _   ) = []
  getName' (c   : rest) = c : getName' rest


main :: IO ()
main = do
  s : opts   <- Env.getArgs
  (opts', _) <- compilerOptions opts
  sourceCode <- readFile s
  rawAst     <- parserStep opts' s sourceCode
  ast        <- calculoEscapadas rawAst opts'
  when (optArbol opts') (showExp ast)
  -- x :: (Either Symbol ([Frag], [([Tree.Stm], Frame)]))
  let (x, i) = evalState (templabRel ast) 0
  when (optIR opts')     (either print (putStrLn . renderCanon) x)
  when (optInterp opts') (either print (runInterp False . correctFrame) x)
  when (optDbg opts')    (either print (runInterp True . correctFrame) x)
  let finalcodeE = x >>= \(strs, procs) -> return $ makeCode strs procs i
  let fn         = L.take (L.length s - 4) s
  when (optAssem opts') (either print (writeCode fn (optM opts')) finalcodeE)

  return ()



toFrame :: IF.Frame -> Frame
toFrame f = Frame
  { TigerFrame.name = IF.name f
  , formals         = IF.formals f
  , locals          = IF.locals f
  , actualReg       = IF.actualReg f
  , actualArg       = IF.actualArg f
  , actualLocal     = IF.actualLocal f
  , maxParams       = IF.maxParams f
  }

toIFFrame :: Frame -> IF.Frame
toIFFrame f = IF.Frame
  { IF.name        = TigerFrame.name f
  , IF.formals     = formals f
  , IF.locals      = locals f
  , IF.actualReg   = actualReg f
  , IF.actualArg   = actualArg f
  , IF.actualLocal = actualLocal f
  , IF.maxParams   = maxParams f
  }

correctFrame :: (t, [(t1, Frame)]) -> (t, [(t1, IF.Frame)])
correctFrame = (\(fr, xs) -> (fr, (\(s, f) -> (s, toIFFrame f)) <$> xs))
