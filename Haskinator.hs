-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menu de opciones, y las funcionalidades basicas
-- de Haskinator.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import Data.Char
import System.IO
import System.Directory (doesFileExist)
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
                                                                                                    putStrLn "Agrega una nueva pregunta  por  favor"
                                                                                                    putStrLn "(Que sea  verdadera   para   la nueva" 
                                                                                                    putStrLn "prediccion  y falsa para la anterior)"
                                                                                                    nuevaPreg <- getLine 
                                                                                                    let x = crearPrediccion nuevaPred
                                                                                                        orcModificado = crearPregunta nuevaPreg  x opos   
                                                                                                        orcADevolver =  Pregunta preg orcModificado oneg
                                                                                                    return  (orcADevolver)
                                                                                            else return (Pregunta preg resp oneg)
                                                                                                 
                                                                              "No" -> do resp <- predecir' oneg      
                                                                                         if resp == oneg    
                                                                                            then do --stuff  
                                                                                                    putStrLn "Respuesta correcta : "
                                                                                                    nuevaPred <- getLine
                                                                                                    putStrLn "Agrega una nueva pregunta  por  favor"
                                                                                                    putStrLn "(Que sea  verdadera   para   la nueva" 
                                                                                                    putStrLn "prediccion  y falsa para la anterior)"
                                                                                                    nuevaPreg <- getLine 
                                                                                                    let x = crearPrediccion nuevaPred
                                                                                                        orcModificado = crearPregunta nuevaPreg  x oneg   
                                                                                                        orcADevolver = Pregunta preg opos orcModificado
                                                                                                    return ( orcADevolver ) 
                                                                                            else return (Pregunta preg opos resp )  
                                                                                                 
                                                                              otherwise -> do putStrLn "Su respuesta debe ser: (Si / No) " 
                                                                                              predecir' orc                     
                                 predecir' ( Prediccion pred )         = do putStr pred
                                                                            putStrLn "( Acertada / Incorrecta )?"
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Acertada"   -> return (Prediccion "Correcta" ) 
                                                                              "Incorrecta" -> return (Prediccion pred)    
                                                                              otherwise    -> do putStrLn "Su respuesta debe ser ( Acertada / Incorrecta )"
                                                                                                 predecir' (Prediccion pred)
                                                                              
calcularEspacio :: Int -> String                                                                               
calcularEspacio 0 = ""  
calcularEspacio 1 = "  " 
calcularEspacio n = "  " ++ calcularEspacio (n-1) 

imprimirOraculo :: Oraculo -> Int -> String
imprimirOraculo ( Prediccion xs ) n  =  xs 
imprimirOraculo ( Pregunta xs opos oneg ) n =  (calcularEspacio n ) ++ "Pregunta: "++ xs ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo positivo: " ++ (imprimirOraculo opos (n+1))  ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo negativo: " ++ (imprimirOraculo oneg (n+1))  

                                 

persistir :: Maybe Oraculo -> IO ()                             
persistir Nothing        = putStrLn "Tratando de guardar un oraculo vacio." 
persistir (Just oraculo) = do 
  putStr "Introduzca el nombre del archivo en donde\nse guardara el oraculo: "
  filename <- getLine
  writeFile filename (show oraculo)
  return ()
	 

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
              
cargar :: IO (Maybe Oraculo)
cargar = do 
  putStr "Introduzca el nombre del archivo en donde\nse encuentra el oraculo: "
  filename <- getLine
  existe <- doesFileExist filename
  if existe then do
    oraculo <- readFile filename
    return (readMaybe oraculo)
  else do
    putStrLn "El archivo no existe."
    return (Nothing)


obtenerPosicion :: [(String,Bool)] -> [(String,Bool)] -> Int
obtenerPosicion []  _  = 0    
obtenerPosicion _   [] = 0  
obtenerPosicion (x:xs) (y:ys) =  if ( fst x == fst y ) 
                                 then 1 + obtenerPosicion xs ys 
                                 else 0       

consultarPreguntaCrucial:: String -> String -> Maybe Oraculo -> String                                 
consultarPreguntaCrucial preg1 preg2 maybeorc = case maybeorc of  Nothing -> "Oraculo vacio"
                                                                  Just orc-> let resp1 = obtenerCadena orc preg1    
                                                                                 resp2 = obtenerCadena orc preg2 
                                                                             in    
                                                                                 case (resp1,resp2) of (Nothing ,_)           -> " Consulta invalida, la primera prediccion no existe "  
                                                                                                       (_,Nothing)            -> " Consulta invalida, la segunda prediccion no existe " 
                                                                                                       ((Just r1),(Just r2))  -> fst (r1 !! ((obtenerPosicion r1 r2)-1))  
consultarPreguntaCrucial _ _ _ = error "Error" 

consultarEstadisticas:: Maybe Oraculo -> IO( (Int,Int,Float) ) 
consultarEstadisticas Nothing = do putStrLn "Oraculo vacio, imposible consultar estadisticas"  
                                   return (-1,-1,-1.0)    
consultarEstadisticas (Just orc)  = return (obtenerEstadisticas orc )                                                  

menu :: Maybe Oraculo -> IO (Maybe Oraculo)
menu oraculoPrincipal = do
    putStrLn " \n\nElige alguna opcion: " 
    putStrLn " 1. Crear un oraculo nuevo " 
    putStrLn " 2. Predecir "
    putStrLn " 3. Persistir "
    putStrLn " 4. Cargar " 
    putStrLn " 5. Consultar pregunta crucial " 
    putStrLn " 6. Consultar estadisticas \n\n"
    opcion <- getLine
    case opcion of  
        "1" -> do
                 putStrLn "Creando oraculo ..\n" 
                 resp <- crearOraculo  
                 putStrLn "Oraculo nuevo y vacio creado. " 
	         menu  ( resp )
        "2" -> do
                 putStrLn "Empezo la prediccion.. \n" 
                 resp <- predecir oraculoPrincipal   
                 putStrLn "Se termino de predecir. " 
	         menu  ( resp )         
        "3" -> do   
                 putStrLn "Guardando el Oraculo en un archivo..\n" 
                 persistir oraculoPrincipal
                 putStrLn "Se termino de guardar el Oraculo." 
	         menu  (oraculoPrincipal )         
        "4" -> do
                 putStrLn "Cargando el Oraculo desde un archivo..\n " 
                 resp <- cargar     
                 putStrLn "Se termino de cargar el Oraculo." 
	         menu  ( resp )         
        "5" -> do
                 putStrLn "Para consultar la pregunta crucial es necesario que "  
                 putStrLn "proveas a Haskinator de dos predicciones. " 
                 putStrLn "Primera prediccion : "  
                 pred1 <- getLine 
                 putStrLn "Segunda prediccion : " 
                 pred2 <- getLine 
                 putStrLn "Consultando...\n" 
                 let resp = consultarPreguntaCrucial  pred1 pred2 oraculoPrincipal 
                 putStr " La pregunta crucial es : " 
                 putStrLn resp                      
	         menu  ( oraculoPrincipal )         
        "6" -> do
                 putStrLn "Consultando estadisticas .. \n" 
                 resp <- consultarEstadisticas oraculoPrincipal  
                 putStr "Estadisticas (Min,Max,Promedio) : "
                 print resp 
                 putStrLn "" 
	         menu  ( oraculoPrincipal  )         


		
main :: IO ()                        
main = do
    menu Nothing
    return ()