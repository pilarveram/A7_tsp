#Instalamos los paquetes necesarios:

#(1)

install.packages('deSolve')

#Importamos las librerías:

library(tidyverse)
library(deSolve)
library(gridExtra)

#(2)

#Parámetros:

pars  <- c(A = 2,      #[m^2]
           R = 1.5,    #[min/m^2]
           fi = 1.332) #[m^3/min]

estanqueNivel <- function(t,h,pars) {
  
  #Extraemos los parámetros
  
  A <- pars['A']
  R <- pars['R']
  fi <- pars['fi']
  
  #Modelo
  
  dh <- fi/A-h/(R*A)
  
  list(dh)
}

#Tiempo y condición inicial

times <- seq(0, 20, by = 0.5)
xstart <- c(h=0)

#ODE

ode(func=estanqueNivel, 
    y=xstart, 
    times = times, 
    parms = pars
    )%>%
  as.data.frame() -> out

out %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='tiempo [min]',y='Altura [m]')

#(3)

#Función de Costos:

funcionCostos <- function(datosexp, Fi) {
  
  #Parámetros:
  
  pars  <- c(A = 2,   #[m^2]
             R = 1.5, #[min/m^2]
             fi = Fi) #[m^3/min]
  
  ode(func=estanqueNivel, 
      y=xstart, 
      times = times, 
      parms = pars
  )%>%
    as.data.frame() -> out
  
  return((sum(A7$h-out$h))^2)
}

#(5)

#Barrido 

#Buscando manualmente un rango para Fi
funcionCostos(A7,1.332)
#error = 0.0008010407

#Proponemos desde 0 a 1.4 con paso 0.001
Fi = seq(0, 1.4, by=0.001)
i = 0
lista = c()

#Iteración
while (i<length(Fi)){
  lista <- append(lista,funcionCostos(A7,Fi[i]))
  i <- i+1
}

#Eliminamos NA
eliminar_NA <- function(x) {
  return(x [! is.na (x)])
}

lista <- eliminar_NA(lista)

#(6)

#Buscamos el mínimo error de la lista
min(lista)
#min = 0.0005492945

#Buscamos el índice correspondiente:
which(lista<0.0006)
#índice = 1332

#Buscamos a qué flujo corresponde
Fi[1332]
#flujo = 1.331 

#(7)

df_E_Fi <- data.frame(
  'Error' = lista,
  'Fi' = Fi[1:1400]
)

df_h_he_t <- data.frame(
  'h_exp' = A7$h,
  'h_mod' = out$h,
  't' = A7$t
)

df_hmod <- data.frame(
  'h_mod' = out$h
)

plot1 <- ggplot(data = df_E_Fi, aes(y = Error, x = Fi)) +
  geom_line(aes(colour = "Error de ajuste")) +
  labs(x = 'Fi [m^3/min]', y = 'Error de ajuste') + theme_bw() +
  scale_color_manual(name = 'Error: 0.0005492945 \nFi: 1.331', values="blue")

plot2 <- ggplot(data = df_h_he_t, aes(t)) +
  geom_point(aes(y=h_exp, colour = "Altura experimental")) + 
  geom_line(aes(y=h_mod , colour = "Altura del modelo"))+
  labs(x = 'Tiempo [min]', y = 'Altura [m]') + theme_bw() +
  scale_color_manual(values = c("green","red"), name = 'Error: 0.0005492945 \nFi: 1.331\nCor: 0.992961')

grid.arrange(plot1,plot2, ncol=2)

#Coeficiente de correlación entre h_mod y h_exp
cor(df_h_he_t$h_exp,df_h_he_t$h_mod)
