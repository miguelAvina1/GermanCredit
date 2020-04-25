# Curso Sistemas Inteligentes.
# Prof Luis Eduardo Falcón
# Feb-Jun 2020

# German credit card UCI data
# http://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)

# Estaremos trabajando con el archivo de la UCI: german.data
# Si lo abren con un editor de texto, verán que no tiene encabezados (header),
# las columnas están separadas por espacios en blanco (sep) y tiene caracteres 
# alfanuméricos para denotar los niveles de algunas variables.

mydata <- read.csv("G:/Mi unidad/data/german.data", 
                   header=FALSE, 
                   sep='', 
                   stringsAsFactors=TRUE)
str(mydata)
head(mydata)

# VARIABLE CATEGORICA: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# A manera de ejemplo, hagamos el análisis de la variable V4-Purpose.
# De los siguientes resultados podemos observar que dicha variable es
# categórica con 10 niveles y varios de dichos niveles tienen menos
# del 10% de datos.

str(mydata$V4)
levels(mydata$V4) # niveles
prop.table(table(mydata$V4))

# Aplicaremos el criterio de agrupar todos aquellos niveles que tengan
# una proporción menor al 10%. Viendo la página de la UCI en la liga de
# arriba (para el significado de cada nivel), podemos agrupar todas las 
# variables menores al 10% en la categoría A410 (others):

mydata$V4 <- factor(mydata$V4,
                    labels=c("A40","A41","A410","A42","A43","A410","A410","A410","A410","A410"))


# Si obtienes de nuevo la información de la variable V4, verás que
# el número de niveles se redujo a 5 y cada nivel tiene ahora un valor
# mayor o igual a 0.10, como se deseaba.

# También podemos asignar el nombre a cada nivel de la variable V4
# de acuerdo al significado de cada una de ellas dada en la página de
# la UCI. Una manera de hacerlo es cambiar los nombres siguiendo el
# orden en el cual están los niveles de la variable. 
# En este caso sería:

levels(mydata$V4)
levels(mydata$V4)[1] <- "car_new"
levels(mydata$V4)[2] <- "car_used"
levels(mydata$V4)[3] <- "others"
levels(mydata$V4)[4] <- "furniture"
levels(mydata$V4)[5] <- "TV"

# Opcional: Otra manera de cambiar los nombres de los niveles
#           es usando el comando ifelse.
#           Nota: Recuerda que tecleando en la consola ?ifelse 
#                 puedes obtener más información de este comando.
#           Así, usamos el comando ifelse para cambiar los nombres, en este caso sería:
#mydata$V4 <- as.factor(ifelse(mydata$V4=="A40","car_new",
#                              ifelse(mydata$V4=="A41","car_used",
#                                     ifelse(mydata$V4=="A42","furniture",
#                                            ifelse(mydata$V4=="A43","TV", "others")))))

# Finalmente podemos cambiar el nombre de la variable V4 de acuerdo
# a la información de la UCI:

names(mydata)[names(mydata)=="V4"] <- "Purpose" 
names(mydata)

# VARIABLE NUMERICA: ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Veamos a manera de ejemplo ahora el caso de una variable numérica.
# En este caso veamos la variable V2, el tiempo en meses del crédito solicitado.

summary(mydata$V2)
hist(mydata$V2)
boxplot(mydata$V2, main="Meses del crédito solicitado",xlab="V2",ylab="meses")

# De lo anterior vemos que el histograma tiene una forma algo irregular,
# aunque podemos decir que está sesgada positivamente.
# Del diagrama de caja observamos algunos datos extremos (outliers).
# En este caso usaremos el criterio de eliminar todos aquellos datos
# que se encuentren más allá del valor Q3+2.5*IQR:

# Calculamos  el umbral con respecto con Q3, en este caso:

umbral <- as.numeric(quantile(mydata$V2, 0.75) + 2.5*IQR(mydata$V2))

# Identificamos los datos de V2 que satisfacen este criterio:

u <- ifelse(mydata$V2 > umbral, TRUE, FALSE ) 
sum(u) # podemos contabilizarlos.
sum(u)/length(u) # Proporción de outliers de V2.

# Podemos ahora eliminar todo el renglón del data.frame que contiene
# dichos outliers y obtener de nuevo el boxplot:

mydata <- mydata[!u, ]  
boxplot(mydata$V2)

# R utiliza el criterio Q1-1.5*IQR y Q3+1.5*IQR para dibujar los bigotes
# y por lo tanto para dibujar los outliers en la figura. Puedes verificar
# que el procedimiento que hicimos en este caso para cancelar algunos
# valores extremos, redujo los outliers en la figura de un 7% a un 5.7%.

# Finalmente podemos asignarle el nombre a la variable V2:

names(mydata)[names(mydata)=="V2"] <- "Months_duration"
names(mydata)

