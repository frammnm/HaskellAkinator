-- Archivo: Haskinator.hs
-- Este archivo contiene el main del programa, el menu de opciones, y las funcionalidades basicas
-- de Haskinator.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

import Data.Char
import System.IO
import Text.Read (readMaybe)
import System.directory (doesFileExist)
import Data.Maybe
import Control.Monad
import Oraculo     



crearOraculo :: IO (Maybe Oraculo) 
crearOraculo =  return (Nothing)        

Ramas = (Bool,Oraculo) 

moverDerecha :: [Ramas] -> Oraculo -> [Ramas]   
moverDerecha xs (Pregunta str opos oneg ) =   


moverIzquierda ::  [Ramas] -> Oraculo -> [Ramas]    
  
  
devolver ::   [Ramas] -> Oraculo -> [Ramas] 
  
predecir :: Maybe Oraculo -> IO Maybe Oraculo
predecir maybeOraculo = case maybeOraculo of 
                             Nothing  -> do putStrLn "Oraculo vacio"
                                            putStrLn "Inserte su nueva pregunta "
                                            nuevaPreg <- getLine
                                            putStrLn "Respuesta correcta para la pregunta :"
                                            nuevaPredPos <- getLine
                                            putStrLn "Respuesta incorrecta para la pregunta :"
                                            nuevaPredNeg <- getLine 
                                            putStrLn "Informacion agregada." 
                                            return  ( Just crearPregunta nuevaPreg (crearPrediccion nuevaPredPos) (crearPrediccion nuevaPredNeg) )  
                                            
                             Just orc -> do predecir' orc 
                               where      
                                 predecir' ( Pregunta preg opos oneg ) = do putStr preg ++ " "  
                                                                            putStrLn "(Si/No)" 
                                                                            resp <- getLine
                                                                            case resp of 
                                                                              "Si" ->  resp <-     
                                                                                       if (predecir'  opos) == Nothing  
                                                                                          then do --leer cosas  
                                                                                                   return ( Just orc ) 
                                                                                          else          
                                                                                               
                                                                              "No" -> predecir (Just oneg)
                                 predecir' ( Prediccion pred )         = do putStr pred
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
                                                                                                 crearPregunta nuevaPreg nuevaPred (Prediccion pred) 
                                                                                                 let padre = obtenerNodo orc (obtenerCadena orc pred 
                                                                                                 putStrLn " Aqui aun no se termina "  
                                                                                                 
                                                                              
calcularEspacio :: Int -> String                                                                               
calcularEspacio 0 = ""  
calcularEspacio 1 = "  " 
calcularEspacio n = "  " ++ calcularEspacio (n-1) 

imprimirOraculo :: Oraculo -> Int -> String
imprimirOraculo ( Prediccion xs ) n  =  xs 
imprimirOraculo ( Pregunta xs opos oneg ) n =  (calcularEspacio n ) ++ "Pregunta: "++ xs ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo positivo: " ++ (imprimirOraculo opos n+1)  ++ "\n" ++  (calcularEspacio n ) ++ "Oraculo negativo: " ++ (imprimirOraculo oneg n+1)  

persistir :: Maybe Oraculo -> IO ()                             
persistir Nothing        = putStrLn "Tratando de guardar un oraculo vacio." 
persistir (Just oraculo) = do 
  putStr "Introduzca el nombre del archivo en donde\nse guardara el oraculo: "
  filename <- getLine
  writeFile filename (show oraculo)
  return ()
	 
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
                             
obtenerPosicion :: [(Bool,String)] -> [(Bool,String)] -> Int   
obtenerPosicion (x:xs) (y:ys) =  if ( snd x == snd y ) 
                                     then 1 + obtenerPosicion xs ys 
                                     else 0       

consultarPreguntaCrucial:: String -> String -> Maybe Oraculo -> String                                 
consultarPreguntaCrucial preg1 preg2 maybeorc = case maybeorc of  Nothing -> putStrLn "Oraculo vacio"
                                                                  Just orc-> let resp1 = obtenerCadena orc preg1    
                                                                                 resp2 = obtenerCadena orc preg2 
                                                                             in    
                                                                                 case (resp1,resp2) of (Nothing ,   _   )     -> " Consulta invalida, la primera pregunta no existe "  
                                                                                                       (   _    ,Nothing)     -> " Consulta invalida, la segunda pregunta no existe " 
                                                                                                       ((Just r1),(Just r2))  -> snd (r1 !! (obtenerPosicion r1 r2))     

                                               

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
    if opcion == "1" then
            resp <- crearOraculo 
	    menu  ( resp )
	else if opcion == "2" then
            resp <- oraculoPrincipal    
	    menu ( predecir resp )		
	else if opcion == "3" then
	    menu (Just (crearPrediccion opcion))
    else
	    menu Nothing
		
main :: IO ()                        
main = do
    menu Nothing
    return ()