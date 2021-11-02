{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module FileHandler where

import Data.List.Split (splitOneOf)

--Funcion que crea una lista de sublistas apartir de una lista de strings
--Entradas: lista de strings, contador, lista vacia
--Salida: lista de sublistas de strings
--Restricciones: debe respetar el tipo de cada parametro y no ser vacío
parsearLista (lista, cont, res) = do
  if (cont == (Prelude.length lista))
    then res
    else do
      let porSeparar = lista !! cont
      let str = splitOneOf "," porSeparar
      parsearLista (lista, cont + 1, res ++ [str])

split p1 p2 = func p1 p2 [[]]
  where
    func p1 [] p3 = Prelude.reverse $ Prelude.map (Prelude.reverse) p3
    func p1 (p2 : pn) (p3 : px) = if p2 == p1 then func p1 pn ([] : (p3 : px)) else func p1 pn ((p2 : p3) : px)

--Función que realiza una tabulación para mostrar un texto en idea de tabla
--Entradas: Un string y un contador iniciando en 0
--Restricciones: string no mayor a 20 caracteres
--Salida: un string
tabular_aux (text, cont) = do
  if (cont >= 20)
    then do
      text
    else do
      tabular_aux (text ++ " ", cont + 1)

tabular text = do tabular_aux (text, Prelude.length text)

--Función que imprime una sublista
--Entradas: una lista de strings y un contador iniciando en 0
--Salida: impresión de los elementos de la lista
--Restricciones: debe ser una lista de string
printSubLista lista cont = do
  if (cont == Prelude.length lista)
    then do
      putStr ("\n")
    else do
      putStr (tabular (lista !! cont))
      putStr ("\t")
      let contador = cont + 1
      printSubLista lista contador
