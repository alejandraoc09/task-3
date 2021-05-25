
#Taller de R: estadistica y programacion
#Jueves, 23 de mayo del 2021
#----------------------------------------------------------------------------------
#Nombres y codigos:
#                  -> Alejandra Orellanos Camargo // 201913577
#                  -> Maria Jose Rivera Chavez // 201914133
#----------------------------------------------------------------------------------

#Cargar y / o instalar paquetes
pacman :: p_load(readxl,haven,dplyr,plyr,data.table,sf,tidyverse,skimr,
                 leaflet,ggsn,ggspatial,plm,AER,margins,stargazer,outreg,
                 arm,rockchalk,plyr,XML,rvest,xml2)
 
#Configurar el WorkSpace
setwd("C:\\Users\\User\\Desktop\\Task 3")
getwd() #Verificamos la ruta de trabajo

#---------------------------------Taller A---------------------------------------

# 1) Datos espaciales
## 1.1. Importar datos espaciales
### 1.1.1

#Cargamos las vias
via <- st_read("data\\input\\VIAS.shp")
#Cargamos los servicios publicos
puntos <- st_read("data\\input\\MGN_URB_TOPONIMIA.shp")

###1.1.2

#Filtramos el objeto puntos
c_medico <- puntos %>% dplyr :: filter(CSIMBOL == '021001' |
                                       CSIMBOL == '021002' |
                                       CSIMBOL == '021003' )
head(c_medico) #Observamos el objeto creado 

###1.1.3 

#Cargamos los centros poblados y filtramos los municipios del Norte de Santander
c_poblado <- readRDS(file = "data\\input\\c poblado (2017).rds") %>%
             filter(cod_dane >= 54001 & cod_dane < 55000)

#Cargamos los departamentos y filtramos el Norte de Santander
depto <- readRDS(file = "data\\input\\dp deptos (2017).rds") %>%
         filter(name_dpto == 'NORTE DE SANTANDER')

#Cargamos la victimas por MAP o MUSE y filtramos los eventos ocurridos en el 
#Norte de Santander
mapmuse <- readRDS(file = "data\\input\\victimas_map-muse.rds") %>%
             filter(cod_mpio >= 54001 & cod_mpio < 55000)

##1.2. Atributos de los objetos

ls() #Miramos una lista de lo que hay en la memoria

skim(c_medico)
skim(c_poblado)
skim(depto)

#De las variables cualitativas de esta data es interesante observar sus tablas
#de frecuencia
skim(mapmuse)
mapmuse$tipo_accidente %>% table()
mapmuse$condicion %>% table()
mapmuse$genero %>% table()
mapmuse$estado %>% table()
mapmuse$actividad %>% table()

skim(puntos)
skim(via)

##1.3 Geometrias del objeto
###1.3.1

c_medico %>% st_bbox() ; c_medico %>% st_crs()

c_poblado %>% st_bbox() ; c_poblado %>% st_crs()

depto %>% st_bbox() ; depto %>% st_crs()

mapmuse %>% st_bbox() ; mapmuse %>% st_crs()
 
Puntos %>% st_bbox() ; Puntos %>% st_crs()

via %>% st_bbox() ; via %>% st_crs()

#Nota: importante tomar en cuenta que todos los objetos estan en un sistema de 
#      coordenas WGS 84, la cual es una de las mas usadas.

###1.3.2

#Nota: para reproyectar los archivos, crearemos un nuevo objeto que almacene
#      los archivos reproyectados, lo anterior para conservar los objetos con
#      el sistema de coordenadas originales.

#Creamos un nuevo objeto con el archivo reproyectado
c_medico_t <- c_medico %>% 
              st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
c_medico_t %>% st_crs() ; c_medico_t %>% st_bbox

#Creamos un nuevo objeto con el archivo reproyectado
c_poblado_t <- c_poblado %>% 
               st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
c_poblado_t %>% st_crs() ; c_poblado_t %>% st_bbox()

#Creamos un nuevo objeto con el archivo reproyectado
depto_t <- depto %>% 
           st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
depto_t %>% st_crs() ; depto_t %>% st_bbox()

#Creamos un nuevo objeto con el archivo reproyectado
mapmuse_t <- mapmuse %>% 
            st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
mapmuse_t %>% st_crs() ; mapmuse_t %>% st_bbox()

#Creamos un nuevo objeto con el archivo reproyectado
Puntos_t <- Puntos %>% 
            st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
Puntos_t %>% st_crs() ; Puntos_t %>% st_bbox()

#Creamos un nuevo objeto con el archivo reproyectado
via_t <- via %>% 
            st_transform(crs = "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#Verificamos los atributos del objeto creado
via_t %>% st_crs() ; via_t %>% st_bbox()

##1.4. Operaciones geometricas
###1.4.1

#Creamos un archivo que sea la interseccion de los eventos violentos y el 
#poligono de Norte de Santander, con lo cual nos quedamos solo con los eventos
#violentos ocurridos dentro del departamento Norte de Santander

mapmuse_depto <- st_intersection(x = mapmuse,y = depto)

#Observamos el objeto creado
ggplot() + 
geom_sf(data = depto,fill = 'gray95',col = "black") +
geom_sf(data = mapmuse_depto,col = "green") +
theme_bw()

###1.4.2

#Seleccionamos el mpio de Abrego, para el cual tenemos un centro poblado
Abrego <- c_poblado %>% filter(cod_dane == '54003')

#Observamos el centro poblado de Abrego
ggplot(Abrego) + geom_sf()

#Creamos una objeto con las vias dentro de este mpio
via_Abrego <- st_intersection(x = via,y = Abrego)
via_Abrego

#Observamos la vias de Abrego
ggplot() +
geom_sf(data = Abrego) + 
geom_sf(data = via_Abrego)

#Ahora usamos el archivos de las vias dentro de Abrego, para medir el largo
#de las vias dentro de este centro poblado

via_Abrego <- via_Abrego %>% mutate(Largo = st_length(via_Abrego)) %>%
                             arrange(desc(Largo))
via_Abrego

#Miramos la cinco vias mas larga dentro del mpio de Abrego
ggplot() +
geom_sf(data = Abrego) + 
geom_sf(data = via_Abrego) +
geom_sf(data = via_Abrego[1:5,],color = 'red',size = 1.5)

##1.5 Pintar mapas
###1.5.1

leaflet(data = depto) %>% 
addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
addPolygons(fill = FALSE, stroke = TRUE, color = "black") %>%
addPolygons(data = c_poblado,fill = TRUE,stroke = TRUE,color = 'red') %>%
addCircleMarkers(data = c_medico,radius = 2.5,color = 'black') 

###1.5.2

#Creamos una varaible dummy que nos permita identificar los lugares
c_poblado <- mutate(c_poblado,Lugar = factor(c('Centros Poblados')))
c_medico <- mutate(c_medico,Lugar = factor(c('Centros medicos')))

Mapa_1 <- ggplot() + 
          geom_sf(data = depto,fill = 'white',color = 'black') +
          geom_sf(data = c_poblado,aes(fill = Lugar,colour = Lugar)) +   
          geom_sf(data = c_medico,aes(fill = Lugar,colour = Lugar),size = 2,shape = 1) +
          labs(title = "Poblados y centros medicos del Norte de Santander",
               caption = "Fuente: Elaboración propia basado en datos del DANE", 
               x = 'Longitud',
               y = 'Latitud') +
          theme(panel.background = element_rect(fill = "white"), #Color del plot
          plot.background = element_rect(fill = "white"), #Color del fondo del plot
          plot.title = element_text(hjust = 0.5, #Titulo del mapa centrado
                                   size = 16,    #Tamaño del titulo
                                   family = "serif", #Fuente de letra
                                   face = "bold"),   #Tipo de letra
          panel.grid = element_blank(), #Eliminar cuadricula del mapa
          legend.key.size = unit(0.5,"cm"), #Tamaño de la leyenda
          legend.key.width = unit(0.8,'line'),
          legend.position = c(0.2,0.15)) + #Lugar de la leyenda 
          annotation_scale()   +    #Este comando añade la escala en el mapa
          annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_nautical, 
                         height = unit(2.5, "cm"),
                         width = unit(2.5, "cm"))

Mapa_1 #Observamos el mapa

#Guardamos el mapa en extension pdf
ggsave(filename = "./views/Mapa 1.pdf",
       plot = Mapa_1,
       width = 20,
       height = 15,
       unit = 'cm')

#2) Regresiones
##2.1

df_mapmuse <- readRDS(file = "data\\output\\f_mapmuse.rds")
names(df_mapmuse) #Revisamos las variables de dataframe

#Hacemos tablas de frecuencia de las variables dummy
apply(df_mapmuse[,c(1,4,5,6,11)],2,table) 

#Nota: dado que la variable tipo de actividad tiene muchas categorias, esta
#      variable no se incluira en el modelo de regresion.

#Definimos la formula del modelo

Formula_1 <- fallecido ~ factor(tipo_accidente) + factor(condicion) + 
                         factor(genero) + dist_hospi + 
                         dist_cpoblado + dist_vias
ols <- lm(formul = Formula_1,data = df_mapmuse) ; ols
summary(ols) #Observamos la salida del modelo

##2.2

#Cargamos la funcion coefplot de la web
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")

#Graficamos y exportamos la figura
png(file = "./views/Coeficientes.png")
coefplot(ols)
dev.off()

##2.3

#Estimamos y observamos el modelo Logit 
Logit <- glm(formula = Formula_1,data = df_mapmuse, family = binomial(link = "logit")) 
Logit %>% summary()

#Estimamos y obervamos el modelo Probit
Probit <- glm(formula = Formula_1,data = df_mapmuse, family = binomial(link = "probit")) 
Probit %>% summary()

##2.4
list_models = list(ols,Logit,Probit)
Tabla_1 <- outreg :: outreg(setNames(list_models, c('ols','logir','probit')))
write.csv(Tabla_1,'table/Tabla 1.csv',row.names = F)

##2.5

#Graficamos y exportamos el efecto marginal para el logit
png(file = "./views/Efecto Logit.png")
cplot(Logit,"dist_hospi")
dev.off()

#Graficamos y exportamos el efecto marginal para el probit
png(file = "./views/Efecto Probit.png")
cplot(Probit,"dist_hospi")
dev.off()

# 3) Web-Scraping

###3.1) 

#Observamos la pagina en el servidor
browseURL(url = 'https://es.wikipedia.org/wiki/Departamentos_de_Colombia',browser = getOption('browser'))

#Creamos un objeto que tenga el HTML de la pagina
Direccion <- 'https://es.wikipedia.org/wiki/Departamentos_de_Colombia'
myhtml = read_html(Direccion)
class(myhtml)

###3.2)

Titulo <- myhtml %>% 
          html_nodes(xpath = '//*[@id="firstHeading"]') %>% 
          html_text() # Convertir en texto
Titulo

###3.3)

#Extraer tablas de un HTML usando el paquete XML
parse <- read_html(Direccion) %>% htmlParse()

#Extraer todas las tablas
tablas <- parse %>% readHTMLTable(header = T)

Departamentos <- tablas[[4]]
View(Departamentos)


