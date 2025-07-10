#CORRELACIONES CANONICAS

#EJERCICIO DECATHON
#paquetes a utilizar
library(CCA)
library(CCP)

#estandarizacion
DC2_E=scale(DC2)

#base de datos para cada grupo de variables
DC2_E_X=DC2_E[,c(1,4,5,7)] #-> carrera
DC2_E_Y=DC2_E[,c(2,3,6)]   #-> salto

#a. Ho: R = ID -> Matriz de correlaciones identica
#   H1: r=/ ID
#calculo de las Correlaciones Canonicas
cc_DC2=cc(DC2_E_X,DC2_E_Y)

#coeficientes de las var C para hacer las ecuaciones
cc_DC2$xcoef #-> Ui
cc_DC2$ycoef #-> Vi
#si es negativo, y aumenta una unidad, cambia inversamente
#si es positivo y aumenta una unidad, incrementa

#correlacines canonicas
cc_DC2$cor 

#Prueba de Wilks
r=cc_DC2$cor
n=23 #Valores obs
p=4  #variables
q=3 #En cuantos grupos dividimos el modelo 
p.asym(r,n,p,q,tstat="Wilks") 
#Analizar el p-value
#PH -> H0: P1=P2=P3=0
#          P2=P3=0
#          P3=0
#Si se va analizando cada hipotesis hasta que > 0.05
#Ahi el resto es = 0 (no hay correlacion)
"
1 -> 0.02982365 < 0.05
2 -> 0.45892323 > 0.05
3 -> 0.59120556 > 0.05
Nos quedamos con la correlacion 1, el resto es 0"

#correlaciones dimension 1 con variables
library(corrplot)

#inferencia en X
U1=cc_DC2$scores$xscores[,1]
cor(U1,DC2_E_X)
#Se miran las correlaciones, si es muy cercana a 0 no sirve, 15%
#La 3 es de 0.16, sospechamos que es mala

DC2_X=data.frame(U1,DC2_E_X)
I_X=cor.mtest(DC2_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)
#Se mira que se relacione, que los valores sean de <alpha, <0.05, si no se quita

#Intervalos de confianza
#si los intervalos de comfianza contienen el 0 no sirve

#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V1=cc_DC2$scores$yscores[,1]
cor(V1,DC2_E_Y)
#Se miran las correlaciones, si es muy cercana a 0 no sirve, 15% ???
#La 3 es de --0.14, sospechamos que es mala

DC2_Y=data.frame(V1,DC2_E_Y)
I_Y=cor.mtest(DC2_Y,conf.level=0.95)

#valores-p (solo la 1ra var C)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)

#Para realizar el resumen: 
#Se mira si la correlacion es inversa(negativa) -> se mira el coeficiente 
#Las variables que descartamos podemos decir que no tienen una influencia relevante en el modelo
#Puntuaciones altas en los saltos corresponden a puntajes altos en las carreras


#EJERCICIO CONTAMINACION
#paquetes a utilizar
library(CCA)
library(CCP)
#estandarizacion
C_E=scale(C)
C_E=as.data.frame(C_E)
#base de datos para cada grupo de variables
C_E_X=C_E[,c(1,4,5,6)]
C_E_Y=C_E[,c(2,3)]
#calculos de las CC
cc_CE=cc(C_E_X,C_E_Y)
#correlaciones canonicas
cc_CE$cor
#Prueba de Wilks
#PH -> H0: P1=P2=0
r=cc_CE$cor
n=41
p=4
q=2
p.asym(r,n,p,q,tstat="Wilks")
#Cuando analizamos el p-valor, nos damos cuenta de que desde la primera correlacion
#no se rechaza H0 entonces tiene correlacion = 0, NO HAY NADA QUE ANALIZAR 


#EJERCICIO VINOS
#paquetes a utilizar
library(CCA)
library(CCP)

#estandarizacion
vinE=scale(vino)

#bases de datos para cada grupo

vinX=vinE[,c(4,5,6)]
vinY=vinE[,c(1,2,3,7)]

#calculos de las CC
cc_vino=cc(vinX,vinY)

#coeficientes de las var C
cc_vino$xcoef
cc_vino$ycoef


#correlaciones canonicas
cc_vino$cor

#Prueba de Wilks
r=cc_vino$cor
n=1599 #variables
p=3    #
q=4
p.asym(r,n,p,q,tstat="Wilks")

#correlaciones con cada dimensión
library(corrplot)

#Recordar
#calculos de las CC
#cc_vino=cc(vinX,vinY)
#bases de datos para cada grupo
#vinX=vinE[,c(4,5,6)]
#vinY=vinE[,c(1,2,3,7)]

#variables canonicas 1

#inferencia en X
U1=cc_vino$scores$xscores[,1]
cor(U1,vinX)

vin_X=data.frame(U1,vinX)
I_X=cor.mtest(vin_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V1=cc_vino$scores$yscores[,1]
cor(V1,vinY)

vin_Y=data.frame(V1,vinY)
I_Y=cor.mtest(vin_Y,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)

#variables canonicas 2

#inferencia en X
U2=cc_vino$scores$xscores[,2]
cor(U2,vinX)

vin_X=data.frame(U2,vinX)
I_X=cor.mtest(vin_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V2=cc_vino$scores$yscores[,2]
cor(V2,vinY)

vin_Y=data.frame(V2,vinY)
I_Y=cor.mtest(vin_Y,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)


#EJERCICIO CONTAMINACION
attach(C)

#paquetes a utilizar
library(CCA)
library(CCP)
#estandarizacion
C_E=scale(C)
C_E=as.data.frame(C_E)b
#base de datos para cada grupo de variables
C_E_X=C_E[,c(1,4,5,6)]
C_E_Y=C_E[,c(2,3)]
#calculos de las CC
cc_CE=cc(C_E_X,C_E_Y)
#correlaciones canonicas
cc_CE$cor
#Prueba de Wilks
#PH -> H0: P1=P2=0
r=cc_CE$cor
n=41
p=4
q=2
p.asym(r,n,p,q,tstat="Wilks")
#Cuando analizamos el p-valor, nos damos cuenta de que desde la primera correlacion
#no se rechaza H0 entonces tiene correlacion = 0, NO HAY NADA QUE ANALIZAR