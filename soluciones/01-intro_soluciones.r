
# (a) generar vector que concatene tres 1, tres 2, tres 3, ..., tres 10 -- ver rep() 
# solucion:
rep(1:10,each=3)

# (b) generar matriz 2x4 con 2 en col1, 4 en col2, 6 en col3 y 8 en col4 -- ver matrix()
# solucion:
matrix(seq(2,8,2), nr=2, nc=4, byrow=T)

# (c) Generar vector tamaño=20 con valores al azar entre 1 y 10
# (c.1) Filtrar elementos ubicados en indice par
# (c.2) Filtrar elementos mayores a 8. ¿en que posiciones estan?
# (c.3) Obtener todos los elementos menos el primero y el ultimo
# solucion:
aa <- sample(1:10,20,rep=T)
aa[seq(2,length(aa),2)]
aa[aa>8]; which(aa>8)
aa[-c(1,length(aa))]

# (d) Generar data.frame al azar con 3 variables --edad, altura, peso-- y 20 observaciones
# (d.1) ¿qué observaciones --por indice-- tienen edad mayor a 40 y peso menor a 70?
# (d.2) generar 4ta columna con nombres de las observaciones (letras distintas)
# (d.3) ¿qué nombres tienen altura mayor a 190 o peso mayor a 80?
# (d.4) generar columna con IMC (peso/altura^2)
# solucion:
df = data.frame(edad = sample(16:80,20,rep=T),
                peso = sample(40:100,20,rep=T),
                altura = sample(140:200,20,rep=T))
df[df$edad>40 & df$peso<70,]; which(df$edad>40 & df$peso<70)
df$nombre = LETTERS[1:20]
df[df$altura>190 | df$peso>80, "nombre", drop=F]
df$imc = df$altura/(df$peso^2)

# (e) Generar una lista de 2 elementos: el vector de (c) y el data.frame de (d)
# (supongamos que el vector de (c) indica las notas en un examen de cada nombre de (d))
# (e.1) Modificar 5 notas al azar con numeros al azar
# (e.2) Asignar nombres al azar a las notas usando las letras del data.frame
# (e.3) Obtener peso y altura de los nombres con nota mayor que 8
# solucion: 
library(magrittr)
milista = list(a=aa, b=df)
milista$a[sample(length(milista$a),5)] = sample(1:10,5)
names(milista$a) = sample(milista$b$nombre, 20)
nombres = milista$a[milista$a>8] %>% names()
milista$b[milista$b$nombre %in% nombres,c("peso","altura")]

# (f) Crear una funcion que al ser aplicada sobre una lista indique:
#     - la clase de cada elemento, y
#       - si es un vector o matriz numerica: el maximo
#       - si es un data.frame: el minimo cada variable
#       - cualquier otro caso: NA
#     Aplicarla a la lista de (e)
# solucion:
info = function(objeto) {
  out = list()
  out$clase = class(objeto)
  if (is.numeric(objeto)) {
    out$info = max(objeto)
  } else {
    if (is.data.frame(objeto)) {
      # out$info = apply(objeto, MARGIN=2, min)
      out$info = purrr::map(objeto, min)
    } else {
      out$info = NA
    }
  }
  return(out)
}
info_lista = function(lista) purrr::map(lista, info)
info_lista(milista)

# (g) Por medio de simulaciones calcular la media de la siguiente variable aleatoria:
# cantidad de veces que hay que sumarle a 1 un numero aleatorio entre 1 y 1000 para
# conseguir una suma divisible por k (hacerlo para k=1,2,3,4,...,10)
# solucion:
func_div = function(k) {
  i = 1
  out = c(1)
  while (sum(out) %% k != 0) {
    out = c(out,sample(1000,1))
    i = i + 1
  }
  return(i)
}

func_div_n = function(k, n) {
  r = replicate(n, func_div(k), simplify=T)
  return(r)
}

muestras = list()
for (i in 1:10) muestras[[i]] = func_div_n(i, 1000)

medias = c()
for (i in seq_along(muestras)) medias[i] = mean(muestras[[i]])
