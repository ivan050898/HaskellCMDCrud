{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes where

import Data.Int (Int64)
import Data.Text
import Database.SQLite.Simple -- manejo de sqlite
import Database.SQLite.SimpleErrors (runDBAction) -- manejo de erorres sqlite
import FileHandler

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Data types
data Parqueo = Parqueo --datype o struct Parqueo
  { nombreP :: Text,
    direccionP :: Text,
    provinciaP :: Text,
    ubicacionX :: Int64,
    ubicacionY :: Int64
  }
  deriving (Eq, Read, Show)

data Empresa = Empresa --datype o struct Empresa
  { nombreE :: Text,
    websiteE :: Text,
    contactoE :: Text,
    pedalE :: Int64,
    electricoE :: Int64
  }
  deriving (Eq, Read, Show)

data Bicicleta = Bicicleta --datype o struct Bicicleta
  { idB :: Text,
    tipoB :: Text,
    parqueoB :: Text,
    estadoB :: Int64
  }
  deriving (Eq, Read, Show)

data Usuario = Usuario --datype o struct Usuario
  { cedulaU :: Int64,
    nombreU :: Text
  }
  deriving (Eq, Read, Show)

data Alquiler = Alquiler --datype o struct Alquiler sin el id alquiler, se usa para insertar
  { salidaA :: Text,
    llegadaA :: Text,
    bicicletaA :: Text,
    estadoA :: Int64,
    usuarioA :: Int64
  }

data AlquilerP = AlquilerP --datype o struct Alquiler sin el id alquiler, se usa para consultar
  { idAP :: Int64,
    salidaAP :: Text,
    llegadaAP :: Text,
    bicicletaAP :: Text,
    estadoAP :: Int64,
    usuarioAP :: Int64
  }
  deriving (Eq, Read, Show)

data Factura = Factura --datype o struct Factura sin el id alquiler, se usa para insertar
  { alquilerF :: Int64,
    kilometrosF :: Int64,
    totalF :: Int64,
    estadoF :: Int64
  }
  deriving (Eq, Read, Show)

data FacturaP = FacturaP --datype o struct Factura sin el id alquiler, se usa para consultar
  { idFP :: Int64,
    alquilerFP :: Int64,
    kilometrosFP :: Int64,
    totalFP :: Int64,
    estadoFP :: Int64
  }
  deriving (Eq, Read, Show)

data IdAlquiler = IdAlquiler --datype o struct IdAlquiler se usa para consultar el ultimo alquiler insertado
  {idAlquiler :: Int64}
  deriving (Eq, Read, Show)

data IdFactura = IdFactura --datype o struct IdFactura se usa para consultar la ultima Factura insertada
  {idFactura :: Int64}
  deriving (Eq, Read, Show)

data Top5Parqueos = Top5Parqueos --datype o struct Top5Parqueos se usa para consultar la estadistica de los 5 parqueos con mas viajes
  { nombreParqueoTop :: Text,
    cantidadParqueoTop :: Int64
  }
  deriving (Eq, Read, Show)

data Top5Usuarios = Top5Usuarios --datype o struct Top5Usuarios se usa para consultar la estadistica de los 5 usuarios con mas viajes
  { cedulaUsuarioTop :: Int64,
    nombreUsuarioTop :: Text,
    cantidadUsuarioTop :: Int64
  }
  deriving (Eq, Read, Show)

data Top3Bicicletas = Top3Bicicletas --datype o struct Top3Bicicletas se usa para consultar la estadistica de las 3 bicicletas con mas kilometros recorridos
  { idBicicletaTop :: Text,
    cantidadkmBicicletaTop :: Int64
  }
  deriving (Eq, Read, Show)

data Resumen = Resumen --datype o struct Resumen se usa para consultar la estadistica resumen
  { cantidadViajesResumen :: Int64,
    cantidadkmResumen :: Int64,
    cantidadpagarBicicletaTop :: Int64
  }
  deriving (Eq, Read, Show)

data CantidadViajesResumenConsulta = CantidadViajesResumenConsulta --datype o struct Resumen se usa para consultar la cantidad de viajes en la  estadistica resumen
  { viajescantidadconsulta :: Int64
  }
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets de la empresa e instancias
getNombreEmpresa (Empresa nombre _ _ _ _) = nombre --devuelve el nombre de la empresa

getWebsiteEmpresa (Empresa _ website _ _ _) = website --devuelve el website de la empresa

getContactoEmpresa (Empresa _ _ contacto _ _) = contacto --devuelve el contacto de la empresa

getPedalEmpresa (Empresa _ _ _ pedal _) = pedal --devuelve el precio del kilometro de bicicleta tradicional de  la empresa

getElectricoEmpresa (Empresa _ _ _ _ electrico) = electrico --devuelve el precio del kilometro de bicicleta electrica de  la empresa

instance FromRow Empresa where --parseador de la base de datos
  fromRow = Empresa <$> field <*> field <*> field <*> field <*> field

instance ToRow Empresa where --parseador hacia la base de datos
  toRow (Empresa nombre website contacto pedal electrico) = toRow (nombre, website, contacto, pedal, electrico)

printEmpresas :: Empresa -> IO () --imprime una fila de la empresa
printEmpresas elemento = do
  let nombre = getNombreEmpresa (elemento)
  let website = getWebsiteEmpresa (elemento)
  let contacto = getContactoEmpresa (elemento)
  let pedal = getPedalEmpresa (elemento)
  let electrico = getElectricoEmpresa (elemento)
  let lista = [unpack nombre, unpack website, unpack contacto, show pedal, show electrico]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--get de parqueos e instancias
getNombreParqueo (Parqueo nombre _ _ _ _) = nombre --devuelve el nombre del parqueo

getDireccionParqueo (Parqueo _ direccion _ _ _) = direccion --devuelve la direcccion del parqueo

getProvincia (Parqueo _ _ provincia _ _) = provincia --devuelve la provincia del parqueo

getXParqueo (Parqueo _ _ _ x _) = x --devuelve el x del parqueo

getYParqueo (Parqueo _ _ _ _ y) = y --devuelve el y del parqueo

instance FromRow Parqueo where --parseador de la base de datos
  fromRow = Parqueo <$> field <*> field <*> field <*> field <*> field

instance ToRow Parqueo where --parseador hacia la base de datos
  toRow (Parqueo nombre direccion provincia x y) = toRow (nombre, direccion, provincia, x, y)

printParqueos :: Parqueo -> IO () --imprime una fila de parqueos
printParqueos elemento = do
  let nombre = getNombreParqueo (elemento)
  let direccion = getDireccionParqueo (elemento)
  let provincia = getProvincia (elemento)
  let x = getXParqueo (elemento)
  let y = getYParqueo (elemento)
  let lista = [unpack nombre, unpack direccion, unpack provincia, show x, show y]
  printSubLista lista 0

insertarParqueo pnombre pdireccion pprovincia px py = do
  --insertar un parqueo
  let nombre = pack pnombre
  let direccion = pack pdireccion
  let provincia = pack pprovincia
  let x = px
  let y = py
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Parqueo VALUES (?,?,?,?,?)" (Parqueo nombre direccion provincia x y)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets de las bicicletas e instancias
getIdBicicleta (Bicicleta id _ _ _) = id --devuelve el identificador de la bicicleta

getTipoBicicleta (Bicicleta _ tipo _ _) = tipo --devuelve el tipo de la bicicleta

getParqueoBicicleta (Bicicleta _ _ parqueo _) = parqueo --devuelve el parqueo de la bicicleta

instance FromRow Bicicleta where --parseador de la base de datos
  fromRow = Bicicleta <$> field <*> field <*> field <*> field

instance ToRow Bicicleta where --parseador hacia la base de datos
  toRow (Bicicleta identificador tipo parqueo estado) = toRow (identificador, tipo, parqueo, estado)

printBicicletas :: Bicicleta -> IO () --imprime una fila de las bicicletas
printBicicletas elemento = do
  let id = getIdBicicleta (elemento)
  let tipo = getTipoBicicleta (elemento)
  let parqueo = getParqueoBicicleta (elemento)
  let lista = [unpack id, unpack tipo, unpack parqueo]
  printSubLista lista 0

insertarBicicleta pId pTipo pParqueo = do
  --insertar una bicicleta
  let id = pack pId
  let tipo = pack pTipo
  let parqueo = pack pParqueo
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Bicicleta VALUES (?,?,?,?)" (Bicicleta id tipo parqueo 0)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--gets del usuario e instancias
getCedulaUsuario (Usuario cedula _) = cedula --devuelve la cedula del usuario

getNombre (Usuario _ nombre) = nombre --devuelve el nombre del usuario

instance FromRow Usuario where --parseador de la base de datos
  fromRow = Usuario <$> field <*> field

instance ToRow Usuario where --parseador hacia la base de datos
  toRow (Usuario cedula nombre) = toRow (cedula, nombre)

printUsuarios :: Usuario -> IO () --imprime una fila de usuarios
printUsuarios elemento = do
  let cedula = getCedulaUsuario (elemento)
  let nombre = getNombre (elemento)
  let lista = [show cedula, unpack nombre]
  printSubLista lista 0

insertarUsuario pcedula pnombre = do
  --insertar un usuario
  let cedula = pcedula
  let nombre = pack pnombre
  conn <- open "PR2.db"
  result <- runDBAction $ execute conn "INSERT into Usuario VALUES (?,?)" (Usuario cedula nombre)
  close conn

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
--parseadores de txt a base de datos

getUsuarioInsertar :: [[Char]] -> IO () --inserta los usuarios desde una lista de 2 dimensiones
getUsuarioInsertar lista = do
  let cedula = lista !! 0
  let cedulaParseada = read cedula :: Int64
  let nombre = lista !! 1
  insertarUsuario cedulaParseada nombre

getParqueoInsertar :: [[Char]] -> IO () --inserta los Parqueos desde una lista de 2 dimensiones
getParqueoInsertar lista = do
  let nombre = lista !! 0
  let direccion = lista !! 1
  let provincia = lista !! 2
  let x = read (lista !! 3) :: Int64
  let y = read (lista !! 4) :: Int64
  insertarParqueo nombre direccion provincia x y

getBicicletaInsertar :: [[Char]] -> IO () --inserta los Parqueos desde una lista de 2 dimensiones
getBicicletaInsertar lista = do
  let id = lista !! 0
  let tipo = lista !! 1
  let ubicacion = lista !! 2
  insertarBicicleta id tipo ubicacion

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Alquiler where --parseador de la base de datos
  fromRow = Alquiler <$> field <*> field <*> field <*> field <*> field

instance ToRow Alquiler where --parseador hacia la base de datos
  toRow (Alquiler salidaA llegadaA bicicletaA estadoA usuarioA) = toRow (salidaA, llegadaA, bicicletaA, estadoA, usuarioA)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow AlquilerP where --parseador de la base de datos
  fromRow = AlquilerP <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow AlquilerP where --parseador hacia la base de datos
  toRow (AlquilerP idAP salidaA llegadaA bicicletaA estadoA usuarioA) = toRow (idAP, salidaA, llegadaA, bicicletaA, estadoA, usuarioA)

getIdAlquiler (AlquilerP id _ _ _ _ _) = id --devuelve el id del alquiler

getSalidaAlquiler (AlquilerP _ salida _ _ _ _) = salida --devuelve el  parqueo de salida del alquiler

getLlegadaAlquiler (AlquilerP _ _ llegada _ _ _) = llegada --devuelve el  parqueo de llegada del alquiler

getBicicletaAlquiler (AlquilerP _ _ _ bicicleta _ _) = bicicleta --devuelve la  bicicleta del alquiler

getUsuarioAlquiler (AlquilerP _ _ _ _ _ usuario) = usuario --devuelve el  usuario del alquiler

printAlquileres :: AlquilerP -> IO () --imprime una fila de los alquileres
printAlquileres elemento = do
  let id = getIdAlquiler (elemento)
  let salida = getSalidaAlquiler (elemento)
  let llegada = getLlegadaAlquiler (elemento)
  let bicicleta = getBicicletaAlquiler (elemento)
  let lista = [show id, unpack salida, unpack llegada, unpack bicicleta]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Factura where --parseador de la base de datos
  fromRow = Factura <$> field <*> field <*> field <*> field

instance ToRow Factura where --parseador hacia la base de datos
  toRow (Factura alquilerF kilometrosF totalF estadoF) = toRow (alquilerF, kilometrosF, totalF, estadoF)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------
instance FromRow FacturaP where --parseador de la base de datos
  fromRow = FacturaP <$> field <*> field <*> field <*> field <*> field

instance ToRow FacturaP where --parseador hacia la base de datos
  toRow (FacturaP idFP alquilerFP kilometrosFP totalFP estadoFP) = toRow (idFP, alquilerFP, kilometrosFP, totalFP, estadoFP)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

getIdFactura (FacturaP id _ _ _ _) = id --devuelve el id de la Factura

getAlquilerFactura (FacturaP _ alquiler _ _ _) = alquiler --devuelve el alquiler de la Factura

getKilometrosFactura (FacturaP _ _ kilometros _ _) = kilometros --devuelve los kilometros recorridos  de la Factura

getTotalFactura (FacturaP _ _ _ total _) = total --devuelve el total a pagar  de la Factura

printFacturas :: FacturaP -> IO () --imprime una fila de las facturas
printFacturas elemento = do
  let id = getIdFactura (elemento)
  let alquiler = getAlquilerFactura (elemento)
  let kilometros = getKilometrosFactura (elemento)
  let total = getTotalFactura (elemento)
  let lista = [show id, show alquiler, show kilometros, show total]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow IdAlquiler where --parseador de la base de datos
  fromRow = IdAlquiler <$> field

getIdAlquiler2 (IdAlquiler id) = id --consigue el id maximo de un alquiler

instance FromRow IdFactura where --parseador hacia la base de datos
  fromRow = IdFactura <$> field

getIdFactura2 (IdFactura id) = id --consigue el id maximo de una  Factura

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top5Parqueos where --parseador de la base de datos
  fromRow = Top5Parqueos <$> field <*> field

getNombreParqueoTop (Top5Parqueos name _) = name --devuelve el nombre del parqueo

getCantidadParqueoTop (Top5Parqueos _ cantidad) = cantidad --devuelve la cantidad de veces que el parqueo fue salida o llegada

printTopParqueos :: Top5Parqueos -> IO () --imprime una fila de la estadistica
printTopParqueos elemento = do
  let name = getNombreParqueoTop (elemento)
  let cantidad = getCantidadParqueoTop (elemento)
  let lista = [unpack name, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top5Usuarios where --parseador de la base de datos
  fromRow = Top5Usuarios <$> field <*> field <*> field

getCedulaUsuarioTop (Top5Usuarios cedula _ _) = cedula --devuelve la cedula del usuario

getNombreUsuarioTop (Top5Usuarios _ nombre _) = nombre --devuelve el nombre del usuario

getCantidadUsuarioTop (Top5Usuarios _ _ cantidad) = cantidad --devuelve la cantidad de alquileres hechos por el usuario

printTopUsuario :: Top5Usuarios -> IO () --imprime una fila de la estadistica
printTopUsuario elemento = do
  let nombre = getNombreUsuarioTop (elemento)
  let cedula = getCedulaUsuarioTop (elemento)
  let cantidad = getCantidadUsuarioTop (elemento)
  let lista = [show cedula, unpack nombre, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Top3Bicicletas where --parseador de la base de datos
  fromRow = Top3Bicicletas <$> field <*> field

getIdBicicletasTop (Top3Bicicletas id _) = id --devuelve el identificador de la bicicleta

getCantidadBicicletasTop (Top3Bicicletas _ cantidad) = cantidad --devuelve la cantidad de kilometros recorridos por la bicicleta

printTopBicicletas :: Top3Bicicletas -> IO () --imprime una fila de la estadistica
printTopBicicletas elemento = do
  let id = getIdBicicletasTop (elemento)
  let cantidad = getCantidadBicicletasTop (elemento)
  let lista = [unpack id, show cantidad]
  printSubLista lista 0

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow Resumen where
  fromRow = Resumen <$> field <*> field <*> field --parseador de la base de datos

getViajesResumen (Resumen viajes _ _) = viajes --devuelve la cantidad de viajes

getKmResumen (Resumen _ km _) = km --devuelve la cantidad de kilometros recorridos por las bicicletas

getTotalResumen (Resumen _ _ total) = total --devuelve la cantidad de colones facturada

printResumen :: Resumen -> IO () --imprime una fila de la estadistica
printResumen elemento = do
  let viajes = getViajesResumen (elemento)
  let km = getKmResumen (elemento)
  let total = getTotalResumen (elemento)
  let lista = [show viajes, show km, show total]
  printSubLista lista 0

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

instance FromRow CantidadViajesResumenConsulta where --parseador de la base de datos
  fromRow = CantidadViajesResumenConsulta <$> field

getCantidadViajesConsulta (CantidadViajesResumenConsulta cantidad) = cantidad --devuelve si hay los datos suficientes para ejecutar la estadistica resumen
