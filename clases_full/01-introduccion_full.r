library(magrittr)

# asignacion --------------------------------------------------------------

x <- 1
x = 1
y = x
y = 4
# comentario

# operadores --------------------------------------------------------------

# aritmeticos
# suma resta multiplicacion resta cociente exponente resto
y + x
y - x
y * x
4 / 8
8 %% 4
2**5
2^5

# relacionales
# < <= > >= == !=
x == 1
x == y
y > x

# logicos
# ! & | && ||
TRUE
!FALSE
!F
F & T
F | T
x == 1 & y==4
x == 1 | y==80
!(x > y); x <= y

# un operador es una funcion binaria (la asignacion tambien es un operador!)


# estructuras de datos -----------------------------------------------

# vectores (atomicos) -----------------------------------------------------

a = c(0,2,5,3,8) # combinar
typeof(a) # tipo
class(a) # clase
b = c("a","b","c","d","e")
typeof(b)
c = c(F,T,F,T,T)
typeof(c)
c(a,b)
typeof(c(a,b)) # los elementos de un vector atomico son del mismo tipo
c(NA,5,2,3,4) # missing
c(NULL,5,2,3,4) # objeto nulo
rm(y) # remove

length(a) # longitud
names(a) # nombres
names(a) = b
names(a) = NULL
a %>% names() # magrittr pipe

rep(5,10) # replicar
seq(1,20) # secuencia
seq_along(a)
1:20
20:1
seq(1,20,1)
seq(1,20,0.5)
set.seed(8) # semilla
muestra = sample(1:10,15,replace=T) # muestra 
sample(letters,10,rep=T)

# operaciones vectorizadas
a*c(1,0,10,1,5)
a==c(0,1,5,0,0)
1:6 + c(10,2) # reciclaje
1:5 + c(10,2) # reciclaje

# subsetting / extraer
# por posicion
a[3] 
a[3:5]
a[c(1,5)]
a[-1]
# por nombre
a[c("a","e")] 
# con booleanos
a[c]
a[a>=4]
a[!(a>=4)]
a[a>2 & a<7]
a[a==0 | a==5]
a[a %in% c(0,5)]
which(a<4) 

# matrices ----------------------------------------------------------------

matrix(rep(3,8)) # matrix
matrix(rep(3,8),nrow=4)
matrix(a, nr=2,nc=4) # reciclaje
cbind(1:5,5:1)
rbind(1:10, letters[10:1])
mat1 = matrix(muestra,nc=5)
dim(mat1) # dimensiones

# subsetting/extraer
mat1[5]
mat1[2,3]
mat1[3,]
mat1[,4]
mat1[,4,drop=F]
mat1[c(1,3),c(1:3,5)]
mat1[-2,-4]

# listas (vectores recursivos) --------------------------------------------

lista1 <- list(mat1,a,b)
str(lista1) # estructura
names(lista1) = c("a","b","c")

# subsetting/extraer
lista1$a
lista1[["a"]]
lista1[[1]]
lista1["a"]
lista1[1]
lista1$c[5]

# data frames --------------------------------------------------------------

df1 = data.frame(mat1)
names(df1) = c("var1","var2","var3","var4","var5")
df2 = data.frame(
  v1=1:50
  ,v2=50:1
  ,v3=sample(1:50,rep=T)
  ,v4=0
  ,v5=sample(letters,50,rep=T)
  ,stringsAsFactors=F
)
str(df2)
dim(df2)
length(df2)
df2$v6 = sample(1:10,50,rep=T)
df2$v6 = NULL

# subsetting/extraer
df2[,4]
df2[,"v4"]
df2$v4
df2[[4]]
df2[4]
df2["v4"]
df2$v5[1:5]
df2[3:8,c(1,5)]
# con booleanos:
df2[df2$v5=="b",]
df2[df2$v2>=41,"v5"]
df2[df2$v2 %in% c(30,50),"v5"]
head(df2,5) # head
tail(df2) # tail

# loops -------------------------------------------------------------------

# for 
for (x in 1:10) {
  print(x*2)
  print("listo")
}
resultado = c()
for (i in seq_along(a)) {
  resultado[i] = a[i]*2
}
  # pregunta: ¿se puede hacer sin loop?

# while
resultado = c()
i = 1 
while (i <= length(a)) {
  resultado[i] = a[i]*2
  i = i+1
}

# purrr::map() 
lista = list(1:10, 20, 56, 13:9)
purrr::map(lista, mean)
resultado = purrr::map(lista, function(x) x+5) # funcion anonima

# EJERCICIO 1: ejecutar esta ultima operacion con while y con for
# solucion:
# i = 1
# resultado = list()
# while (i<=length(lista)) {
#   resultado[[i]] = lista[[i]] + 5
#   i = i + 1
# }
# resultado = list()
# for (i in seq_along(lista)) resultado[[i]] = lista[[i]] + 5

# EJERCICIO 2: simular cuantas veces necesito sumarle a 1 un numero al azar entre 1 y 1000
# para que la suma del conjunto sea divisible por 5
# solucion
# i = 1
# out = c(1)
# while (sum(out) %% 5 != 0) {
#   out = c(out,sample(1000,1))
#   print(paste("iteracion: ", i))
#   i = i + 1
#   print(out)
#   print(paste("resto = ",sum(out) %% 5))
# }

# apply
apply(df1,MARGIN=1,sum)
df1 %>% apply(2, sum)


# condicionales -----------------------------------------------------------

y = 2; f = "ruben"

if (is.numeric(y)) {
  print("es numerico!")
}

if (is.numeric(y)) print("es numerico!")

if (is.numeric(f)) {
  print("es numerico!")
} else {
  print("no es numerico!")
}

ifelse(is.numeric(y), y+8, NA)
# funciona vectorizado
ifelse(a>=5, a+10, 0)

# funciones ---------------------------------------------------------------

# ejemplo: generacion de matrices con numeros al azar
crea_matriz = function(min, max, rows=4, cols=4) {
  nums = sample(min:max, size=rows*cols, replace=T)
  mat = matrix(nums, nrow=rows, ncol=cols)
  return(mat)
}
# ejemplo: operacion condicional sobre una matriz
matriz_fun = function(mat) {
  if (mean(mat)>10) {┴
    return(mat*100)
  } else {
    return(0)
  }
}

matrices = list()
for (i in 1:100) matrices[[i]] = crea_matriz(0,20,5,2)
out = purrr::map(matrices, matriz_fun)

# tambien se puede usar replicate
# matrices = replicate(n=100, crea_matriz(0,20,5,2), simplify=F)


# ejercicios --------------------------------------------------------------

# (a) generar vector que concatene tres 1, tres 2, tres 3, ..., tres 10 -- ver rep() 
# solucion:
# rep(1:10,each=3)

# (b) generar matriz 2x4 con 2 en c1, 4 en c2, 6 en c3 y 8 en c4 -- ver matrix()
# solucion:
# matrix(seq(2,8,2), nr=2, nc=4, byrow=T)

# (c) Generar vector tamaño=20 con valores al azar entre 1 y 10
# (c.1) Filtrar elementos ubicados en indice par
# (c.2) Filtrar elementos mayores a 8. ¿en que posiciones estan?
# (c.3) Obtener todos los elementos menos el primero y el ultimo
# solucion:
# aa <- sample(1:10,20,rep=T)
# aa[seq(2,length(aa),2)]
# aa[aa>8]; which(aa>8)
# aa[-c(1,length(aa))]

# (d) Generar data.frame al azar con 3 variables --edad, altura, peso-- y 20 observaciones
# (d.1) ¿qué observaciones --por indice-- tienen edad mayor a 40 y peso menor a 70?
# (d.2) generar 4ta columna con nombres de las observaciones (letras distintas)
# (d.3) ¿qué nombres tienen altura mayor a 190 o peso mayor a 80?
# (d.4) generar columna con IMC (peso/altura^2)
# solucion:
# df = data.frame(edad = sample(16:80,20,rep=T),
#                 peso = sample(40:100,20,rep=T),
#                 altura = sample(140:200,20,rep=T))
# df[df$edad>40 & df$peso<70,]; which(df$edad>40 & df$peso<70)
# df$nombre = LETTERS[1:20]
# df[df$altura>190 | df$peso>80, "nombre", drop=F]
# df$imc = df$altura/(df$peso^2)

# (e) Generar una lista de 2 elementos: el vector de (c) y el data.frame de (d)
    # (supongamos que el vector de (c) indica las notas en un examen de cada nombre de (d))
# (e.1) Modificar 5 notas al azar con numeros al azar
# (e.2) Asignar nombres al azar a las notas usando las letras del data.frame
# (e.3) Obtener peso y altura de los nombres con nota mayor que 8
# solucion: 
# milista = list(a=aa, b=df)
# milista$a[sample(length(milista$a),5)] = sample(1:10,5)
# names(milista$a) = sample(milista$b$nombre, 20)
# nombres = milista$a[milista$a>8] %>% names()
# milista$b[milista$b$nombre %in% nombres,c("peso","altura")]

# (f) Crear una funcion que al ser aplicada sobre una lista indique:
#     - la clase de cada elemento, y
#       - si es un vector o matriz numerica: el maximo
#       - si es un data.frame: el minimo cada variable
#       - cualquier otro caso: NA
#     Aplicarla a la lista de (e)
# solucion:
# info = function(objeto) {
#   out = list()
#   out$clase = class(objeto)
#   if (is.numeric(objeto)) {
#     out$info = max(objeto) 
#   } else {
#     if (is.data.frame(objeto)) {
#       # out$info = apply(objeto, MARGIN=2, min)
#       out$info = purrr::map(objeto, min)
#     } else {
#       out$info = NA
#     }
#   }
#   return(out)
# }
# info_lista = function(lista) purrr::map(lista, info)
# info_lista(milista)

# (g) Por medio de simulaciones calcular la media de la siguiente variable aleatoria:
    # cantidad de veces que hay que sumarle a 1 un numero aleatorio entre 1 y 1000 para
    # conseguir una suma divisible por k (hacerlo para k=2,3,4,...,10)

# anexo -------------------------------------------------------------------

# "problema" de map vs loop
lista = list(0,0,"1",8)
# con for
resultado = list()
for (i in seq_along(lista)) resultado[[i]] = lista[[i]]+5
# con purrr::map()
resultado2 = purrr::map(lista, function(x) x+5)

