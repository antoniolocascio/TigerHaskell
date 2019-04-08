module TigerInterpTypes where

import           Prelude                 hiding ( (!!) )
import           TigerSymbol
import           TigerInterpFrame
import           TigerTree
import           TigerTemp
import           Data.Map                      as M

import           Data.List                     as L
                                         hiding ( (!!) )

import           Control.Arrow
import           Control.Monad.State
-- | Datos a almacenar en memoria.
data Dato
    -- | String constantes.
    = Str Symbol
    -- | Cuerpos de funciones constantes.
    | FBody (
        -- | Lista de acceso de los posibles argumentos.
        [Access]
        -- | Body de la función.
        , [Stm]
        , Frame)
    -- | O puedo almacenar un entero.
    | DInt Int
    deriving (Show)

-- | Funcinoes auxiliares de proyección con errores significativos
getInt :: Dato -> Int
getStr :: Dato -> Symbol
getFBody :: Dato -> ([Access], [Stm], Frame)

getInt (DInt i) = i
getInt _        = error "NOT AN INT"

getStr (Str s) = s
getStr _       = error "NOT A Symbol?"

getFBody (FBody sts) = sts
getFBody _           = error "NOT A FUN"

data CPU = CPU
    { -- | Mem representa la memoria del CPU, básicamente los registros.
      mem    :: M.Map Temp Int
      -- | Representa la memoria RAM, mapea direcciones de memoria a datos.
    , wat    :: M.Map Int Dato
      -- | Mapea Labels a direcciones de memoria.
    , dat    :: M.Map Label Int
      -- | Buffer de salida, a donde imprime la llamada a print.
    , output :: [Symbol]
      -- | Buffer de entrada, de donde sacamos la entrada cuando thacemos getchar.
    , input  :: [Symbol]
      -- | Stack de nombre de funciones, en el tope esta la actual
    , frameStack :: [Frame]
    , inter :: Bool
    , maxHeap :: Int
    }
    deriving Show

getDat :: Label -> CPU -> Dato
getDat l cpu = wat cpu !! (dat cpu !! l)

uTemp :: Temp -> Int -> CPU -> CPU
uTemp t i cpu = cpu { mem = M.insert t i (mem cpu) }

memSize :: Int
memSize = 2048

unstring :: String -> String
unstring s@('.' : _) = L.init $ L.drop 9 s
unstring s           = s

type RC = StateT CPU IO

-- Map helper test function
(!!) :: (Ord k, Show k, Show v) => Map k v -> k -> v
m !! k = maybe (error $ "No encontrada la clave: " ++ show k) id $ M.lookup k m
