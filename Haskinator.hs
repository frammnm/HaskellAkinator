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

obtenerNodo :: Oraculo -> [(Bool,String)] -> Oraculo
obtenerNodo orc [] = orc 
obtenerNodo (Pregunta str opos oneg ) (x:xs) = if fst ( x ) == True   
                                               then obtenerNodo opos xs    
                                               else obtenerNodo oneg xs      
predecir :: Maybe Oraculo -> Maybe Oraculo 
predecir maybeOraculo = case maybeOraculo of 
                             Nothing  -> do putStrLn "Oraculo vacio"
                                            putStrLn "Inserte su nueva pregunta "
                                            nuevaPreg <- getLine
                                            putStrLn "Respuesta correcta para la pregunta :"
                                            nuevaPredPos <- getLine
                                            putStrLn "Respuesta incorrecta para la pregunta :"
                                            nuevaPredNeg <- getLine 
                                            crearPregunta nuevaPreg (crearPrediccion nuevaPredPos) (crearPrediccion nuevaPredNEg) 
                                            putStrLn "Informacion agregada." 
                                            
                             Just orc -> do predecir' orc 
                               where      
                                 predecir' ( Pregunta preg opos oneg ) = do putStr preg ++ " "  
                                                                            putStrLn "(Si/No)" 
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Si" -> predecir (Just opos)
                                                                              "No" -> predecir (Just oneg)
                                 predecir' ( Prediccion pred )         = do putStr pred
                                                                            putStrLn "( Acertada | Incorrecta )"
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Acertada"   -> Nothing
                                                                              "Incorrecta" -> do -- putStrLn "Respuesta correcta : "
                                                                                                 -- nuevaPred <- getLine
                                                                                                 -- crearPrediccion nuevaPred 
                                                                                                 -- putStr "Agrega una nueva pregunta por   favor"
                                                                                                 -- putStr "(Que sea  verdadera   para   la nueva" 
                                                                                                 -- putStrLn "prediccion  y falsa para la anterior)"
                                                                                                 -- nuevaPreg <- getLine 
                                                                                                 -- crearPregunta nuevaPreg nuevaPred (Prediccion pred) 
                                                                                                 -- let padre = obtenerNodo orc (obtenerCadena orc pred 
                                                                                                 putStrLn " Aqui aun no se termina "  
                                                                                                 
                                                                              
calcularEspacio :: Int -> String                                                                               
calcularEspacio 0 = ""  
calcularEspacio 1 = "  " 
calcularEspacio n = "  " ++ calcularEspacio (n-1) 

imprimirOraculo :: Oraculo -> Int -> String
imprimirOraculo ( Predecir xs ) n  =  
imprimirOraculo ( Pregunta xs opos oneg ) n =  (calcularEspacio n ) ++ "Pregunta: "++ xs ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo positivo: " ++ (imprimirOraculo opos n+1)  ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo negativo: " ++ (imprimirOraculo oneg n+1)  

persistir :: Maybe Oraculo -> String                             
persistir Nothing _  = putStrLn " Tratando de guardar un Oraculo vacio " 
persistir ( Just orc ) name = do handle <- openFile name WriteMode 
                                 hPutStr handle ( imprimirOraculo orc 0 )
                                 
                                 

obtenerPosicion :: [(Bool,String)] -> [(Bool,String)] -> Int   
obtenerPosicion (x:xs) (y:ys) =  if ( snd x == snd y ) 
                                     then 1 + obtenerPosicion xs ys 
                                     else 0       

consultarPreguntaCrucial:: String -> String -> Maybe Oraculo -> String                                 
consultarPreguntaCrucial preg1 preg2 maybeorc = case maybeorc of  Nothing -> putStrLn "Oraculo vacio"
                                                                  Just orc-> let resp1 = obtenerCadena orc preg1    
                                                                                 resp2 = obtenerCadena orc preg2 
                                                                                 case (resp1,resp2) of (Nothing ,   _   )     -> " Consulta invalida, la primera pregunta no existe "  
                                                                                                       (   _    ,Nothing)     -> " Consulta invalida, la segunda pregunta no existe " 
                                                                                                       ((Just r1),(Just r2))  -> snd (r1 !! (obtenerPosicion r1 r2))     

                                               
                                               
                                                                                                               
main = forever $ do  
  putStrLn " Elige alguna opcion: " 
  putStrLn " 1. Crear un oraculo nuevo " 
  putStrLn " 2. Predecir "
  putStrLn " 3. Persistir "
  putStrLn " 4. Cargar " 
  putStrLn " 5. Consultar pregunta crucial " 
  putStrLn " 6. Consultar estadisticas "
  op <-getLine 
  let oraculoPrincipal = crearOraculo 
  case op of 
    "1" -> putStrLn "1"
    "2" -> putStrLn "2"
    "3" -> putStrLn "3"
    "4" -> putStrLn "4"
    "5" -> putStrLn "5"
    "6" -> putStrLn "6"
    other -> putStrLn "No es una opcion valida."
