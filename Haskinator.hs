-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menu de opciones, y las funcionalidades basicas
-- de Haskinator.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import System.IO
import System.Directory (doesFileExist)
import Oraculo     

-- Funcion que devuelve un oraculo vacio.
crearOraculo :: IO (Maybe Oraculo) 
crearOraculo =  return Nothing       

-- Funcion utilizada para obtener una prediccion a partir de un oraculo.
predecir :: Maybe Oraculo -> IO (Maybe Oraculo)
predecir Nothing    = do putStrLn "Oraculo vacio."
                         putStr "Inserte su nueva pregunta: "
                         nuevaPreg <- getLine
                         putStr "Respuesta correcta para la pregunta: "
                         nuevaPredPos <- getLine
                         putStr "Respuesta incorrecta para la pregunta: "
                         nuevaPredNeg <- getLine 
                         putStrLn "Informacion agregada." 
                         let resp = Just (crearPregunta nuevaPreg (crearPrediccion nuevaPredPos) (crearPrediccion nuevaPredNeg)) 
                         return  resp   
predecir (Just orc) = do resp <- (predecir' orc) 
                         return (Just resp)
  where      
    predecir' (Pregunta preg opos oneg) = do putStr "* Pregunta: "
                                             putStr preg  
                                             putStrLn " (Si/No)?" 
                                             resp <- getLine
                                             putStrLn ""
                                             case resp of 
                                               "Si" -> do resp <- predecir' opos      
                                                          if resp == opos then 
                                                            do putStr "Respuesta correcta: "
                                                               nuevaPred <- getLine
                                                               putStrLn "\nPor favor agrega una  nueva pregunta,"
                                                               putStrLn "(que sea  verdadera   para   la nueva" 
                                                               putStrLn "prediccion  y falsa para la anterior)"
                                                               nuevaPreg <- getLine 
                                                               let x = crearPrediccion nuevaPred
                                                                   orcModificado = crearPregunta nuevaPreg  x opos   
                                                                   orcADevolver = Pregunta preg orcModificado oneg
                                                               return orcADevolver
                                                          else return (Pregunta preg resp oneg)
                                               "No" -> do resp <- predecir' oneg
                                                          if resp == oneg then 
                                                            do putStr "Respuesta correcta: "
                                                               nuevaPred <- getLine
                                                               putStrLn "\nPor favor agrega una  nueva pregunta,"
                                                               putStrLn "(que sea  verdadera   para   la nueva" 
                                                               putStrLn "prediccion  y falsa para la anterior)"
                                                               nuevaPreg <- getLine 
                                                               let x = crearPrediccion nuevaPred
                                                                   orcModificado = crearPregunta nuevaPreg  x oneg   
                                                                   orcADevolver = Pregunta preg opos orcModificado
                                                               return orcADevolver 
                                                          else return (Pregunta preg opos resp )  
                                               otherwise -> do putStrLn "Su respuesta debe ser: (Si/No)" 
                                                               predecir' orc                     
    predecir' (Prediccion pred)         = do putStr "* Prediccion: "
                                             putStr pred
                                             putStrLn " (Acertada/Incorrecta)?"
                                             resp <- getLine
                                             putStrLn ""
                                             case resp of 
                                               "Acertada"   -> return (Prediccion "Correcta" ) 
                                               "Incorrecta" -> return (Prediccion pred)    
                                               otherwise    -> do putStrLn "Su respuesta debe ser: (Acertada/Incorrecta)"
                                                                  predecir' (Prediccion pred)

-- Funcion que guarda el oraculo actual en un archivo.                                                                                             
persistir :: Maybe Oraculo -> IO ()                             
persistir Nothing        = putStrLn "Tratando de guardar un oraculo vacio." 
persistir (Just oraculo) = do putStr "Introduzca el nombre del archivo en donde\nse guardara el oraculo: "
                              filename <- getLine
                              writeFile filename (show oraculo)
                              return ()

-- Funcion que utiliza read y retorna un Maybe a.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing
              
-- Funcion que lee un archivo de texto y retorna el oraculo leido. 
cargar :: IO (Maybe Oraculo)
cargar = do putStr "Introduzca el nombre del archivo en donde\nse encuentra el oraculo: "
            filename <- getLine
            existe <- doesFileExist filename
            if existe then 
              do oraculo <- readFile filename
                 let resp = readMaybe oraculo
                 if resp == Nothing then
                   do putStrLn "El contenido del archivo no contiene un oraculo valido."
                      return Nothing
                 else return resp
            else do putStrLn "El archivo no existe."
                    return Nothing

-- Funcion que obtiene la posicion del string que representa la pregunta crucial.
obtenerPosicion :: [(String,Bool)] -> [(String,Bool)] -> Int
obtenerPosicion []  _  = 0    
obtenerPosicion _   [] = 0  
obtenerPosicion (x:xs) (y:ys) = if (fst x == fst y) then 1 + obtenerPosicion xs ys 
                                else 0
            
-- Funcion que dada dos predicciones y un oraculo, retorna la pregunta crucial de ambas.
consultarPreguntaCrucial :: String -> String -> Maybe Oraculo -> String                                 
consultarPreguntaCrucial pred1 pred2 mOraculo = case mOraculo of  
                                                  Nothing  -> "Oraculo vacio, imposible consultar la pregunta crucial."
                                                  Just orc -> let 
                                                                resp1 = obtenerCadena orc pred1
                                                                resp2 = obtenerCadena orc pred2 
                                                              in    
                                                                case (resp1,resp2) of 
                                                                  (Nothing ,_)      -> "Consulta invalida, la primera prediccion no existe."  
                                                                  (_,Nothing)       -> "Consulta invalida, la segunda prediccion no existe." 
                                                                  (Just r1,Just r2) -> fst (r1 !! ((obtenerPosicion r1 r2)-1))
                                  
-- Funcion que utiliza que aplica la funcion obtenerEstadisticas a un maybe oraculo.
consultarEstadisticas :: Maybe Oraculo -> IO ((Int,Int,Float)) 
consultarEstadisticas Nothing     = do putStrLn "Oraculo vacio, imposible consultar estadisticas."  
                                       return (-1,-1,-1.0)    
consultarEstadisticas (Just orc)  = return (obtenerEstadisticas orc)                                                  

-- Funcion utilizada para representar el menu principal de Haskinator.
menu :: Maybe Oraculo -> IO (Maybe Oraculo)
menu oraculoPrincipal = do
  putStrLn "\n\n****************** MENU ******************"
  putStrLn "Elige alguna opcion:" 
  putStrLn " 1. Crear un oraculo nuevo" 
  putStrLn " 2. Predecir"
  putStrLn " 3. Persistir"
  putStrLn " 4. Cargar" 
  putStrLn " 5. Consultar pregunta crucial" 
  putStrLn " 6. Consultar estadisticas"
  putStrLn " 7. Salir\n\n"
  putStr "Opcion escogida: "
  opcion <- getLine
  putStrLn ""
  case opcion of  
    "1" -> do putStrLn "Creando el Oraculo ..\n" 
              resp <- crearOraculo  
              putStrLn "Oraculo vacio creado." 
              menu resp
    "2" -> do putStrLn "Empezo la prediccion ..\n" 
              resp <- predecir oraculoPrincipal   
              putStrLn "Se termino de predecir." 
              menu resp         
    "3" -> do putStrLn "Guardando el Oraculo en un archivo ..\n" 
              persistir oraculoPrincipal
              putStrLn "Se termino de guardar el Oraculo." 
              menu oraculoPrincipal         
    "4" -> do putStrLn "Cargando el Oraculo desde un archivo ..\n" 
              resp <- cargar     
              putStrLn "Se termino de cargar el Oraculo." 
              menu resp         
    "5" -> do putStrLn "Para consultar la pregunta crucial es necesario que"  
              putStrLn "proveas a Haskinator de dos predicciones." 
              putStr "Primera prediccion: "  
              pred1 <- getLine 
              putStr "\nSegunda prediccion: " 
              pred2 <- getLine 
              putStrLn "\nConsultando ..\n" 
              let resp = consultarPreguntaCrucial  pred1 pred2 oraculoPrincipal 
              putStr "La pregunta crucial es: " 
              putStrLn resp                      
              menu oraculoPrincipal         
    "6" -> do putStrLn "Consultando estadisticas ..\n" 
              resp <- consultarEstadisticas oraculoPrincipal  
              putStr "Estadisticas (Min,Max,Promedio): "
              print resp 
              putStrLn "" 
              menu oraculoPrincipal
    "7" -> do putStrLn "Saliendo .. \n"
              return (Nothing)
    otherwise -> do putStrLn "La opcion introducida no es valida.\n"
                    menu oraculoPrincipal

-- Funcion principal del programa.
main :: IO ()                        
main = do
    menu Nothing
    putStrLn "****************** FIN  ******************"
    return ()