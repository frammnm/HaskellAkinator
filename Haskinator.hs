-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menú de opciones, y las funcionalidades básicas
-- de Haskinator.
-- Autores:
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import Data.Char
import Data.Maybe
import Control.Monad
import Oráculo

crearOráculo :: Maybe Oráculo
crearOráculo = Nothing

obtenerNodo :: Oráculo -> [(Bool, String)] -> Oráculo
obtenerNodo orc [] = orc
obtenerNodo (Pregunta str opos oneg ) ((x, _):xs)
  = obtenerNodo (if x then opos else oneg) xs

predecir :: Maybe Oráculo -> Maybe Oráculo
predecir maybeOráculo
  = case maybeOráculo of 
    Nothing -> do
      putStrLn "Oráculo vacio"
      putStrLn "Inserte su nueva pregunta"
      nuevaPreg <- getLine
      putStrLn "Respuesta correcta para la pregunta:"
      nuevaPredPos <- getLine
      putStrLn "Respuesta incorrecta para la pregunta:"
      nuevaPredNeg <- getLine 
      crearPregunta
        nuevaPreg
        (crearPredicción nuevaPredPos)
        (crearPredicción nuevaPredNEg) 
      putStrLn "Información agregada." 

    Just orc ->
      predecir' orc

  where
    predecir' (Pregunta preg opos oneg)
      = do
        putStrLn $ preg ++ " (Sí/No)" 
        resp <- getLine
        case resp of 
          "Sí" -> predecir (Just opos)
          "No" -> predecir (Just oneg)

    predecir' (Predicción pred)
      = do
        putStrLn $ pred ++ " (Acertada | Incorrecta)"
        resp <- getLine
        case resp of
          "Acertada" -> Nothing
          "Incorrecta" -> do
            -- putStrLn "Respuesta correcta:"
            -- nuevaPred <- getLine
            -- crearPrediccion nuevaPred
            -- putStr "Agrega una nueva pregunta por favor (que sea verdadera para la nueva predicciony falsa para la anterior)"
            -- nuevaPreg <- getLine 
            -- crearPregunta nuevaPreg nuevaPred (Predicción pred) 
            -- let padre = obtenerNodo orc (obtenerCadena orc pred 
            putStrLn "Aquí aún no se termina"

calcularEspacio :: Int -> String                                                                               
calcularEspacio 0 = ""
calcularEspacio 1 = "  "
calcularEspacio n = "  " ++ calcularEspacio (n - 1)

imprimirOráculo :: Oráculo -> Int -> String
imprimirOráculo (Predecir xs) n =
imprimirOráculo (Pregunta xs opos oneg) n
  =  calcularEspacio n ++ "Pregunta: " ++ xs++ "\n"
  ++ calcularEspacio n ++ "Oráculo positivo: " ++ imprimirOráculo opos (n + 1) ++ "\n"
  ++ calcularEspacio n ++ "Oráculo negativo: " ++ imprimirOráculo oneg (n + 1)

persistir :: Maybe Oráculo -> String
persistir Nothing _  = putStrLn "Tratando de guardar un oráculo vacío"
persistir (Just orc) name
  = do
    handle <- openFile name WriteMode
    hPutStr handle $ imprimirOráculo orc 0
    -- FIXME: No estás cerrando el archivo!  Usa simplemente «writeFile» en vez de «openFile» y escritura por separado!

obtenerPosición :: [(Bool, String)] -> [(Bool, String)] -> Int
obtenerPosición ((_, x):xs) ((_, y):ys)
  = if x == y
    then 1 + obtenerPosición xs ys
    else 0

consultarPreguntaCrucial :: String -> String -> Maybe Oráculo -> String
consultarPreguntaCrucial preg1 preg2 maybeorc
  = case maybeorc of
    Nothing -> putStrLn "Oráculo vacio"
    Just orc ->
      let
        resp1 = obtenerCadena orc preg1
        resp2 = obtenerCadena orc preg2
      in -- Te faltaba este «in».
        case (resp1, resp2) of
          (Nothing, _      ) -> "Consulta inválida, la primera pregunta no existe"
          (_      , Nothing) -> "Consulta inválida, la segunda pregunta no existe"
          (Just r1, Just r2) -> snd (r1 !! obtenerPosicion r1 r2)

main = forever $ do
  putStrLn " Elige alguna opción:"
  putStrLn " 1. Crear un oráculo nuevo"
  putStrLn " 2. Predecir"
  putStrLn " 3. Persistir"
  putStrLn " 4. Cargar"
  putStrLn " 5. Consultar pregunta crucial"
  putStrLn " 6. Consultar estadísticas"
  op <- getLine
  let oráculoPrincipal = crearOráculo 
  case op of 
    "1" -> putStrLn "1"
    "2" -> putStrLn "2"
    "3" -> putStrLn "3"
    "4" -> putStrLn "4"
    "5" -> putStrLn "5"
    "6" -> putStrLn "6"
    other -> putStrLn "No es una opción válida."
