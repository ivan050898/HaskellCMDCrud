{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.IO

import Data.Int (Int64)
import Data.Text (Text, pack)
import DataTypes
import FileHandler ( parsearLista, split, printListaGeneral )
import Database.SQLite.Simple
  ( close,
    execute,
    open,
    query_,
  )

main :: IO ()
main = do
  putStrLn "Ingrese la ruta del archivo de los usuarios"
  putStr ">>";
  hFlush stdout
  ruta <- getLine
  contenido <- readFile ruta
  let lista = split '\n' contenido
  let valores = parsearLista(lista,0,[])
  mapM_ getBicicletaInsertar valores
  conn <- open "PR2.db"
  q <- query_ conn "SELECT * from Bicicleta" :: IO [Bicicleta]
  close conn
  mapM_ printBicicletas q
