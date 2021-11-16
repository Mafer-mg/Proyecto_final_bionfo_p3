### Proyecto final: Identificación de lepóridos por el tercer premolar inferior ####
# Maria Fernanda Martinez Garcia (264683)

#### Objetivo ####

# Con ayuda de R identificar a qué especie pertenece, su ubicación geográfica, el clima al que estuvo expuesto, 
# su tamaño y tener una idea general acerca de su filogenia, con base en las medidas observadas en el tercer premolar inferior

#### Funcion: Tercer premolar inferior ####

p3_medidas <-function (){
 
  p3_1 <- readline (prompt = "Ingresa tu medida del diámetro mesio-distal (mm):  ")
  p3_2 <- readline (prompt = "Ingresa tu medida del diámetro vestibulo-lingual (mm):  ")
  
  tercer_premolar <- if (p3_1 >= 0.32 & p3_2 >= 0.32 & p3_1 <= 0.42 & p3_2 <= 0.42) { 
    print("Oryctolagus cuniculus") 
  } else if (p3_1 >= 0.32 & p3_2 >= 0.28 & p3_1 <= 0.35 & p3_2 <= 0.34){ 
    print("Lepus c. capensis")
  } else if (p3_1 >= 0.28 & p3_2 >= 0.26 & p3_1 <= 0.35 & p3_2 <= 0.32){ 
    print("Lepus c. centralis")
  } else if (p3_1 >= 0.24 & p3_2 >= 0.23 & p3_1 <= 0.29 & p3_2 <= 0.26){ 
    print("Lepus americanus")
  } else if (p3_1 >= 0.309 & p3_2 >= 0.25 & p3_1 <= 0.39 & p3_2 <= 0.33){ 
    print("Lepus californicus")
  } else if (p3_1 >= 0.32 & p3_2 >= 0.27 & p3_1 <= 0.36 & p3_2 <= 0.33){ 
    print("Lepus townsendii")
  } else if (p3_1 >= 0.23 & p3_2 >= 0.19 & p3_1 <= 0.31 & p3_2 <= 0.28){ 
    print("Sylvilagus audubonii")
  } else if (p3_1 >= 0.20 & p3_2 >= 0.16 & p3_1 <= 0.28 & p3_2 <= 0.22){ 
    print("Sylvilagus bachmani")
  } else if (p3_1 >= 0.22 & p3_2 >= 0.16 & p3_1 <= 0.29 & p3_2 <= 0.24){
    print("Sylvilagus nuttalli")}
 
  return (print (paste("Segun el tercer premolar inferior la especie a la que pertenecen tus medidas es:",tercer_premolar)))
}

#### Funcion: Informacion complementaria ####

especie_info <-function (){
  
  especie <- readline (prompt = "Para obtener infomación complementaria, asi como un árbol filogenetico. 
                       Ingrese el resultado de la especie obtenida anteriormente:  ")
  
  Oryctolagus_cuniculus <- "Oryctolagus_cuniculus"
  Lepus_c._capensis <- "Lepus_c._capensis"
  Lepus_c._centralis <- "Lepus_c._centralis"
  Lepus_americanus <- "Lepus_americanus"
  Lepus_californicus <- "Lepus_californicus"
  Lepus_townsendii <- "Lepus_townsendii"
  Sylvilagus_audubonii <- "Sylvilagus_audubonii"
  Sylvilagus_bachmani <- "Sylvilagus_bachmani"
  Sylvilagus_nuttalli <- "Sylvilagus_nuttalli"
  
  ubicacion <- if (especie == Oryctolagus_cuniculus) { 
    print("Suroeste de Europa") 
  } else if (especie == Lepus_c._capensis){ 
    print("Provincia del Cabo en Sudáfrica")
  } else if (especie == Lepus_c._centralis){ 
    print("Provincia del Cabo en Sudáfrica")
  } else if (especie == Lepus_americanus){ 
    print("Alaska, noroeste de Estados Unidos y Canadá")
  } else if (especie == Lepus_californicus){ 
    print("Suroeste y centro de Estados Unidos; norte de México")
  } else if (especie == Lepus_townsendii){ 
    print("Norte y oeste de Estados Unidos")
  } else if (especie == Sylvilagus_audubonii){ 
    print("Centro y norte de México; sur y norte de Estados Unidos")
  } else if (especie == Sylvilagus_bachmani){ 
    print("California")
  } else if (especie == Sylvilagus_nuttalli){ 
    print("Noroeste y suroeste Estados Unidos")}
  
  clima <- if (especie == Oryctolagus_cuniculus) { 
    print("con vientos moderado, cielos nubosos y lluvias débiles") 
  } else if (especie == Lepus_c._capensis){ 
    print("mediterráneo, en el cual hay veranos calurosos e inviernos húmedos")
  } else if (especie == Lepus_c._centralis){ 
    print("mediterráneo, en el cual hay veranos calurosos e inviernos húmedos")
  } else if (especie == Lepus_americanus){ 
    print("con veranos frescos y mayormente nublados e inviernos largos y helados")
  } else if (especie == Lepus_californicus){ 
    print("de tipo cálido y húmedo, con largas temporadas de sequía")
  } else if (especie == Lepus_townsendii){ 
    print("continental húmedo")
  } else if (especie == Sylvilagus_audubonii){ 
    print("árido o seco")
  } else if (especie == Sylvilagus_bachmani){ 
    print("mediterráneo que puede ser desde árido a subártico")
  } else if (especie == Sylvilagus_nuttalli){ 
    print("que se mantienen relativamente cálido durante todo el año")}
  
  tamannio <- if (especie == Oryctolagus_cuniculus) { 
    print("cercano a 40 cm") 
  } else if (especie == Lepus_c._capensis){ 
    print("cercano a 46.5 - 50 cm")
  } else if (especie == Lepus_c._centralis){ 
    print("cercano a 46.5 - 50 cm")
  } else if (especie == Lepus_americanus){ 
    print("cercano a 42 cm")
  } else if (especie == Lepus_californicus){ 
    print("cercano a 47 – 63 cm")
  } else if (especie == Lepus_townsendii){ 
    print("cercano a 51 cm")
  } else if (especie == Sylvilagus_audubonii){ 
    print("cercano a 33 cm")
  } else if (especie == Sylvilagus_bachmani){ 
    print("cercano a 30 cm")
  } else if (especie == Sylvilagus_nuttalli){ 
    print("cercano a 32 cm")}
  
return (print (paste("La ubicación geográfica en el que se encuentra su especie es",ubicacion, 
                     "; que posee un clima", clima, ".Por lo que tiene un tamaño",tamannio)))
}

#### EXTRA Funcion: Mandibula ####

# Oryctolagus cuniculus, Lepus c. capensis y Lepus c. centralis:
# Son las especies/subespecies de lepóridos más comunes por lo que decidí adicionar medidas de su mandíbula
# para una identificación más precisa. Asi datos que especifiquen en un rango mas corto/pequeño su ubicacion.

medidas_mandibula <-function (){
  
  p3_1_esp <- readline (prompt = "Ingresa tu medida del diámetro mesio-distal (mm):  ")
  p3_2_esp <- readline (prompt = "Ingresa tu medida del diámetro vestibulo-lingual (mm):  ")
  md_1_esp <- readline (prompt = "Ingresa tu medida de la longitud del diastema (mm):  ")
  md_2_esp <- readline (prompt = "Ingresa tu medida de la longitud de la serie dentaria (mm):  ")
  md_3_esp <- readline (prompt = "Ingresa tu medida de la altura de la mandíbula (mm):  ")
  
  tercer_premolar_esp <- if (p3_1_esp >= 0.67 & p3_2_esp >= 0.67 & p3_1_esp <= 0.67 & p3_2_esp <= 0.67) { 
    print("Oryctolagus cuniculus de Lomas, España") 
  } else if (p3_1_esp >= 0.47 & p3_2_esp >= 0.47 & p3_1_esp <= 0.62 & p3_2_esp <= 0.62){ 
    print("Oryctolagus cuniculus de Santarém, Portugal")
  } else if (p3_1_esp >= 0.03 & p3_2_esp >= 0.03 & p3_1_esp <= 0.14 & p3_2_esp <= 0.14){ 
    print("Oryctolagus cuniculus de Navarre, España")
  } else if (p3_1_esp >= 0.13 & p3_2_esp >= 0.26 & p3_1_esp <= 0.13 & p3_2_esp <= 0.26){ 
    print("Oryctolagus cuniculus de Tour du Valat, Francia")
  } else if (p3_1_esp >= 0.32 & p3_2_esp >= 0.28 & p3_1_esp <= 0.35 & p3_2_esp <= 0.34){ 
    print("Lepus c. capensis de Provincia del Cabo, Sudáfrica")
  } else if (p3_1_esp >= 0.28 & p3_2_esp >= 0.26 & p3_1_esp <= 0.35 & p3_2_esp <= 0.32){
    print("Lepus c. centralis de Provincia del Cabo, Sudáfrica")}
  
  mandibula_esp <- if (md_1_esp >= 0.12 & md_2_esp >= 0.33 & md_3_esp >= 0.43 & md_1_esp <= 0.42 & md_2_esp <= 0.44 & md_3_esp <= 0.43) { 
    print("Oryctolagus cuniculus de Lomas, España") 
  } else if (md_1_esp >= 0.21 & md_2_esp >= 0.07 & md_3_esp >= 0.01 & md_1_esp <= 0.42 & md_2_esp <= 0.29 & md_3_esp <= 0.05){ 
    print("Oryctolagus cuniculus de Santarém, Portugal")
  } else if (md_1_esp >= 0.56 & md_2_esp >= 0.83 & md_3_esp >= 0.42 & md_1_esp <= 0.64 & md_2_esp <= 0.83 & md_3_esp <= 0.43){ 
    print("Oryctolagus cuniculus de Navarre, España")
  } else if (md_1_esp >= 0.64 & md_2_esp >= 0.27 & md_3_esp >= 0.13 & md_1_esp <= 0.64 & md_2_esp <= 0.41 & md_3_esp <= 0.26){ 
    print("Oryctolagus cuniculus de Tour du Valat, Francia")
  } else if (md_1_esp >= 0.77 & md_2_esp >= 0.58 & md_3_esp >= 0.37 & md_1_esp <= 0.95 & md_2_esp <= 0.64 & md_3_esp <= 0.42){ 
    print("Lepus c. capensis de Provincia del Cabo, Sudáfrica")
  } else if (md_1_esp >= 0.73 & md_2_esp >= 0.57 & md_3_esp >= 0.35 & md_1_esp <= 0.96 & md_2_esp <= 0.65 & md_3_esp <= 0.41){
    print("Lepus c. centralis de Provincia del Cabo, Sudáfrica")}
  
  return (print (paste("Segun el tercer premolar inferior la especie a la que pertenecen tus medidas es:",tercer_premolar_esp,
                       ".Mientras que, según las medidas de la mandibula pertenece a la especie:",mandibula_esp)))
}

#### Explicacion del scrip ####

# Realice una funcion, que consiste en guardar una serie de pasos en R, que se realizaran despues de colocar el nombre del objeto donde los realizaste: 
# medidas_p3, especie_info y medidas_mandibula en este caso.
# Prumero no coloque nada entre parentesis, porque los datos que necesito se colocan en la funcion readline donde con prompt se le coloca la descripcion.
# Con {} se colocan los argumentos, en este caso el cuerpo del scrip.
# Por ultimo con return va a mostrar el resultado que nosotros pidamos del scrip dado, pero esto sera con un mensaje y de ahi print.

# Despues utilice la funcion con if en donde le di de condicion una serie de medidas para que si son cumplidas arroje el mensaje con print
# ... en todos estos casos use las medidas minimas y maximas, que mencione con: >= y <=. Mientras que todos estos parametros fueron mencionados con &.
# Por lo que, muestra que si con cumplidos algunas de las medidas dentro de esos rangos muestre el resultado que escogi con print.
# Y en caso de que no exista por eso coloque else if, en donde escogeria otra opcion.

#### Arbol filogenetico*** ####

# Como mencione otro objetivo era ver su filogenia, sin embargo, no fue posible:(
# Primero porque las secuencias eran enormes, tardaban alrededor de 30 minutos en descargarse
# Segundo no todos los leporidos que mencione han sido secuenciados
# Tercero, y super obvio, es que mi computadora no puede con tanto:(

# Por lo queee, intente secuencias de proteinas, solo para darnos una idea...
# ... pero ocurrio el mismo problema ademas que no..
# ... encontre una proteina en comun para que fuera significativa la comparacion

# Aun asi dejo un scrip de lo que investigue de como debio de ejecutarse:) (perdon por los problemas)

# Cargar la libreria msa: library (msa)
# Colocar el documento con las secuencias: documento <- readAAStringSet (documento.fasta) 
# Cargarlo: labels (documento)
# Hacer un alineamiento: alineamiento <- msa (documento, "Muscle")
# Convertirlo: convertido<- msaConvert (alineamiento, type="seqinr::alignment")
# Crear una matriz de distancia: matriz_distancia <- dist.alignment (alineamiento, "identity")
# Convertirlo en matriz: as.matrix (matriz_distancia)
# Que aparezcan sus vecinos/ramas: arbol <-nj (matriz_distancia)
# Observarlo en una grafica: plot (arbol)
