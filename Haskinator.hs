-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menu de opciones, y las funcionalidades basicas
-- de Haskinator.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import Data.Char
import System.IO
import Data.Maybe
import Control.Monad
import Oraculo     



crearOraculo :: IO (Maybe Oraculo) 
crearOraculo =  return (Nothing)        

-- Ramas = (Bool,Oraculo) 

-- moverDerecha :: [Ramas] -> Oraculo -> [Ramas]   
-- moverDerecha xs (Pregunta str opos oneg ) =   


-- moverIzquierda ::  [Ramas] -> Oraculo -> [Ramas]    
  
  
-- devolver ::   [Ramas] -> Oraculo -> [Ramas] 
  
predecir :: Maybe Oraculo -> IO (Maybe Oraculo)
predecir maybeOraculo = case maybeOraculo of 
                             Nothing  -> do putStrLn "Oraculo vacio:"
                                            putStrLn "Inserte su nueva pregunta -> "
                                            nuevaPreg <- getLine
                                            putStrLn "Respuesta correcta para la pregunta :"
                                            nuevaPredPos <- getLine
                                            putStrLn "Respuesta incorrecta para la pregunta :"
                                            nuevaPredNeg <- getLine 
                                            putStrLn "Informacion agregada." 
                                            let resp = Just (  crearPregunta nuevaPreg (crearPrediccion nuevaPredPos) (crearPrediccion nuevaPredNeg) ) 
                                            return  resp   
                                            
                             Just orc ->  do   resp <- (predecir' orc) 
                                               return (Just resp) 
                               where      
                                 predecir' ( Pregunta preg opos oneg ) = do putStr (preg ++ " ")  
                                                                            putStrLn "( Si / No )?" 
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Si" ->  do resp <- predecir' opos      
                                                                                          if resp == opos   
                                                                                            then do --stuff  
                                                                                                    putStrLn "Respuesta correcta : "
                                                                                                    nuevaPred <- getLine
                                                                                                    putStr "Agrega una nueva pregunta por   favor"
                                                                                                    putStr "(Que sea  verdadera   para   la nueva" 
                                                                                                    putStrLn "prediccion  y falsa para la anterior)"
                                                                                                    nuevaPreg <- getLine 
                                                                                                    let x = crearPrediccion nuevaPred
                                                                                                        orcModificado = crearPregunta nuevaPreg  x opos   
                                                                                                        orcADevolver =  Pregunta preg orcModificado oneg
                                                                                                    return  (orcADevolver)
                                                                                            else return (orc)
                                                                                                 
                                                                              "No" -> do resp <- predecir' oneg      
                                                                                         if resp == opos   
                                                                                            then do --stuff  
                                                                                                    putStrLn "Respuesta correcta : "
                                                                                                    nuevaPred <- getLine
                                                                                                    putStr "Agrega una nueva pregunta por   favor"
                                                                                                    putStr "(Que sea  verdadera   para   la nueva" 
                                                                                                    putStrLn "prediccion  y falsa para la anterior)"
                                                                                                    nuevaPreg <- getLine 
                                                                                                    let x = crearPrediccion nuevaPred
                                                                                                        y = crearPregunta nuevaPreg  x oneg   
                                                                                                    return (Just ( Pregunta preg y oneg )) 
                                                                                            else return (Just orc) 
                                                                                                 
                                                                              otherwise -> do putStrLn "Su respuesta debe ser: (Si / No) " 
                                                                                              predecir' orc                     
                                 predecir' ( Prediccion pred )         = do putStr pred
                                                                            putStrLn "( Acertada / Incorrecta )?"
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Acertada"   -> return () 
                                                                              "Incorrecta" -> return (Prediccion pred)    
                                                                              otherwise    -> do putStrLn "Su respuesta debe ser ( Acertada / Incorrecta )"
                                                                              
calcularEspacio :: Int -> String                                                                               
calcularEspacio 0 = ""  
calcularEspacio 1 = "  " 
calcularEspacio n = "  " ++ calcularEspacio (n-1) 

imprimirOraculo :: Oraculo -> Int -> String
imprimirOraculo ( Prediccion xs ) n  =  xs 
imprimirOraculo ( Pregunta xs opos oneg ) n =  (calcularEspacio n ) ++ "Pregunta: "++ xs ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo positivo: " ++ (imprimirOraculo opos (n+1))  ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo negativo: " ++ (imprimirOraculo oneg (n+1))  

-- persistir :: Maybe Oraculo -> String -> IO ()                              
-- persistir Nothing _  = putStrLn " Tratando de guardar un Oraculo vacio " 
-- persistir ( Just orc ) name = do handle <- openFile (name WriteMode) 
--                                  hPutStr handle ( imprimirOraculo orc 0 )
                                 
                                 

obtenerPosicion :: [(String,Bool)] -> [(String,Bool)] -> Int   
obtenerPosicion (x:xs) (y:ys) =  if ( fst x == fst y ) 
                                     then 1 + obtenerPosicion xs ys 
                                     else 0       

consultarPreguntaCrucial:: String -> String -> Maybe Oraculo -> String                                 
consultarPreguntaCrucial preg1 preg2 maybeorc = case maybeorc of  Nothing -> "Oraculo vacio"
                                                                  Just orc-> let resp1 = obtenerCadena orc preg1    
                                                                                 resp2 = obtenerCadena orc preg2 
                                                                             in    
                                                                                 case (resp1,resp2) of (Nothing ,   _   )     -> " Consulta invalida, la primera pregunta no existe "  
                                                                                                       (   _    ,Nothing)     -> " Consulta invalida, la segunda pregunta no existe " 
                                                                                                       ((Just r1),(Just r2))  -> fst (r1 !! (obtenerPosicion r1 r2))  

                                               

menu :: Maybe Oraculo -> IO (Maybe Oraculo)
menu oraculoPrincipal = do
    putStrLn " Elige alguna opcion: " 
    putStrLn " 1. Crear un oraculo nuevo " 
    putStrLn " 2. Predecir "
    putStrLn " 3. Persistir "
    putStrLn " 4. Cargar " 
    putStrLn " 5. Consultar pregunta crucial " 
    putStrLn " 6. Consultar estadisticas "
    opcion <- getLine
    case opcion of  
        "1" -> do
                 resp <- crearOraculo  
	         menu  ( resp )
        "2" -> do
                 resp <- predecir oraculoPrincipal   
	         menu  ( resp )         
        "3" -> do   
                 --persistir oraculoPrincipal
	         menu  (oraculoPrincipal )         
        "4" -> do
                 resp <- crearOraculo    
	         menu  ( resp )         
        "5" -> do
                 putStr "Para consultar la pregunta crucial es necesario que "  
                 putStrLn "proveas a Haskinator de dos predicciones. " 
                 putStrLn "Primera prediccion : "  
                 pred1 <- getLine 
                 putStrLn "Segunda prediccion : " 
                 pred2 <- getLine 
                 let resp = consultarPreguntaCrucial  pred1 pred2 oraculoPrincipal 
	         menu  ( oraculoPrincipal )         
        "6" -> do
                 resp <- crearOraculo   
	         menu  ( resp )         


		
main :: IO ()                        
main = do
    menu Nothing
    return ()