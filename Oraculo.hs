-- Archivo: Oraculo.hs
-- Este archivo contiene todas las funciones para la manipulacion del tipo de datos Oraculo.
-- Autores: 
--    - Francisco Martinez 09-10502
--    - Gabriel Alvarez    09-10029

module Oraculo 
( Oraculo (..)  
, crearPrediccion  
, crearPregunta  
, prediccion  
, pregunta  
, positivo
, negativo
, obtenerCadena 
) where

-- Tipo de datos Oraculo.
data Oraculo = Prediccion String | Pregunta String Oraculo Oraculo
	deriving (Show,Read)

-- Funciones de construccion:	
crearPrediccion :: String -> Oraculo
crearPrediccion x = Prediccion x

crearPregunta :: String -> Oraculo -> Oraculo -> Oraculo
crearPregunta x p n = Pregunta x p n

-- Funciones de acceso:
prediccion :: Oraculo -> String
prediccion (Prediccion x) = x
prediccion _              = error "La funcion \"prediccion\" solo se puede aplicar a oraculos de prediccion" 

pregunta :: Oraculo -> String
pregunta (Pregunta x p n) = x
pregunta _                = error "La funcion \"pregunta\" solo se puede aplicar a oraculos de pregunta"

positivo :: Oraculo -> Oraculo
positivo (Pregunta x p n) = p
positivo _                = error "La funcion \"positivo\" solo se puede aplicar a oraculos de pregunta"

negativo :: Oraculo -> Oraculo
negativo (Pregunta x p n) = n
negativo _                = error "La funcion \"negativo\" solo se puede aplicar a oraculos de pregunta"

-- Funciones de modificacion:

-- Funcion que inserta un elemento a una "Just lista".
insertMaybeList :: a -> Maybe [a] -> Maybe [a]
insertMaybeList x Nothing  = Nothing
insertMaybeList x (Just y) = Just (x:y)

obtenerCadena :: Oraculo -> String -> Maybe [(String,Bool)]
obtenerCadena x y = fst (obtenerCadena' x y 0)
	where
		obtenerCadena' (Prediccion x)   y num = if x == y then ((Just []),num)
												else (Nothing,num)
		obtenerCadena' (Pregunta x p n) y num = 	
											let 
												pos = (obtenerCadena' p y (num+1))
												neg = (obtenerCadena' n y (num+1)) 
											in
												case (fst pos,fst neg) of
													(Just _,Nothing) -> (insertMaybeList (x,True) (fst pos),snd pos)
													(Nothing,Just _) -> (insertMaybeList (x,False) (fst neg),snd neg)
													(Just _,Just _)  -> if snd neg >= snd pos then
																			(insertMaybeList (x,True) (fst pos),snd pos)
																		else
																			(insertMaybeList (x,False) (fst neg),snd neg)
													otherwise        -> (Nothing,0)







x = obtenerCadena (crearPregunta "Una Pregunta?" 
								(crearPregunta "Blah1" 
									(crearPrediccion "Blah1.11") 
									(crearPregunta "Blah1.2" 
										(crearPrediccion "Blah1.2.1") 
										(crearPrediccion "Nada")
									)
								)
								(crearPregunta "Blah2" 
									(crearPrediccion "Nada")
									(crearPregunta "Blah2.2" 
										(crearPrediccion "Blah2.2.1") 
										(crearPregunta "Blah2.2.2" 
											(crearPrediccion "Blah2.2.2.1") 
											(crearPrediccion "Nada")
										)
									)
								) 
				   ) "Nada"



