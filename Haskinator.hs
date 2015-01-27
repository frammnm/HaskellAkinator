-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menu de opciones, y las funcionalidades basicas
-- de Haskinator.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import Data.Char
import Data.Maybe
import Control.Monad
import Oraculo   

crearOraculo :: Maybe Oraculo 
crearOraculo = Nothing        
              
predecir :: Maybe Oraculo 
predecir maybeOraculo = case maybeOraculo of 
                             Nothing  -> putStrLn "Oraculo vacio"
                             Just orc -> do predecir' orc 
                               where      
                                 predecir' (Pregunta preg opos oneg) = do putStr preg ++ " "  
                                                                          putStrLn "(Si/No)" 
                                                                          resp <- getLine
                                                                          case resp of 
                                                                            "Si" -> opos
                                                                            "No" -> oneg
                                 predecir' (Prediccion pred)         = do putStr pred
                                                                          putStrLn "( Acertada | Incorrecta )"
                                                                          resp <- getLine
                                                                          case resp of 
                                                                            "Acertada"   -> Nothing
                                                                            "Incorrecta" -> do putStrLn "Respuesta correcta : "
                                                                                               nuevaPred <- getLine
                                                                                               crearPrediccion nuevaPred 
                                                                                               putStr "Agrega una nueva pregunta por   favor"
                                                                                               putStr "(Que sea  verdadera   para   la nueva" 
                                                                                               putStrLn "prediccion  y falsa para la anterior)"
                                                                                               nuevaPreg <- getLine 
                                                                                               crearPregunta nuevaPreg nuevaPred orc 
                            
             
-- predecir (Pregunta preg opos oneg) = do 
--                                      putStr preg ++ " "  
--                                      putStrLn "(Si/No)" 
--                                      resp <- getLine
--                                      case resp of 
--                                        "Si" -> opos
--                                        "No" -> oneg 
                                       
-- predecir (Prediccion pred) = do  
--                              putStr pred
--                              putStrLn "( Acertada | Incorrecta )"
--                              resp <- getLine
--                              case resp of 
--                                "Acertada" -> Nothing
--                                "Incorrecta" -> do   
--                                                  putStrLn "Respuesta correcta : "
--                                                  nuevaPred <- getLine
--                                                  crearPrediccion nuevaPred 
--                                                  putStr "Agrega una nueva pregunta por   favor"
--                                                  putStr "(Que sea  verdadera   para   la nueva" 
--                                                  putStrLn "prediccion  y falsa para la anterior)"
--                                                  nuevaPreg <- getLine 
--                                                  crearPregunta nuevaPreg nuevaPred OraculoPrincipal
                                                 
 
main = forever $ do 
  let OraculoPrincipal = Nothing 
  putStrLn " Elige alguna opcion: " 
  putStrLn " 1. Crear un oraculo nuevo " 
  putStrLn " 2. Predecir "
  putStrLn " 3. Persistir "
  putStrLn " 4. Cargar " 
  putStrLn " 5. Consultar pregunta crucial " 
  putStrLn " 6. Consultar estadisticas "
  op <-getLine 
  case op of 
    "1" -> putStrLn "1"
    "2" -> putStrLn "2"
    "3" -> putStrLn "3"
    "4" -> putStrLn "4"
    "5" -> putStrLn "5"
    "6" -> putStrLn "6"
    other -> putStrLn "No es una opcion valida."
