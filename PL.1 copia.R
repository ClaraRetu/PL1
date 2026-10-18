
(encuesta=read.csv2("abalone.data.csv")) 
gender=encuesta$gender

whole_weight=encuesta$whole.weight
weight_F= whole_weight[gender=="F"]
weight_M=whole_weight[gender=="M"]

# media 
mean(whole_weight)
mean(weight_M)
mean(weight_F)

#mediana
median(whole_weight)
median(weight_F)
median(weight_M)

#mínimo
min(whole_weight)
min(weight_F)
min(weight_M)

#máximo
max(whole_weight)
max(weight_F)
max(weight_M)

#Rango
max(whole_weight)-min(whole_weight)
max(weight_F)-min(weight_F)
max(weight_M)-min(weight_M)

#varianza
var(whole_weight)
var(weight_F)
var(weight_M)

#desviación estandar
sd(whole_weight)
sd(weight_F)
sd(weight_M)

#coeficiente de variación
sd(whole_weight)/mean(whole_weight)
sd(weight_F)/mean(weight_F)
sd(weight_M)/mean(weight_M)

#primer cuartil
quantile(whole_weight,0.25)
quantile(weight_F,0.25)
quantile(weight_M,0.25)

#tercer cuartil
quantile(whole_weight,0.75)
quantile(weight_F,0.75)
quantile(weight_M,0.75)

#rango intercuartílico
IQR(whole_weight)
IQR(weight_F)
IQR(weight_M)

#coeficiente de asimetría
asimetria=function(x){n= length(x); return((1/n)*sum((x-mean(x))^3)/sd(x)^3)} 
asimetria(whole_weight)
asimetria(weight_F)
asimetria(weight_M)
 

#coeficiente de apuntamiento
curtosis=function(x){n=length(x);return((1/n)*sum((x-mean(x))^4)/sd(x)^4 -3)}
curtosis(whole_weight)
curtosis(weight_F)
curtosis(weight_M)

#valores inferiores a la media
sum(whole_weight<mean(whole_weight))
100*mean(whole_weight<mean(whole_weight))

sum(weight_F<mean(weight_F))
100*mean(weight_F<mean(weight_F))

sum(weight_M<mean(weight_M))
100*mean(weight_M<mean(weight_M))

#valores superiores a la media
sum(whole_weight>mean(whole_weight))
100*mean(whole_weight>mean(whole_weight))

sum(weight_F>mean(weight_F))
100*mean(weight_F>mean(weight_F))

sum(weight_M>mean(weight_M))
100*mean(weight_M>mean(weight_M))



#tabla de contingencia (weight/gender)los intervalos se han sacaso con la regla de Sturges.
L=c(0.0,0.3,0.6,0.9,1.2,1.5,1.8,2.1,2.4,2.7)
whole_weight.agrupado=cut(whole_weight,breaks=L, right=FALSE,include.lowest=TRUE)
addmargins(table(whole_weight.agrupado,gender))

# tabla de Contingencia(ligero, medio, pesado).
#se usan los cuartiles ya calculados anteriormente para hacer lo sintervalos de ligero, medio y pesado.
L=c(0.0,quantile(whole_weight,0.25),quantile(whole_weight,0.75),2.55)
whole_weight.intervalos=cut(whole_weight,breaks = L,right = FALSE,include.lowest = TRUE)
addmargins(table(whole_weight.intervalos,gender))


#diagrama de tartas
frec.abs.gender=table(gender)
pct=round(100*prop.table(frec.abs.gender))
pie(frec.abs.gender,labels=paste(names(frec.abs.gender), labels=paste(pct,"%")))


frec.abs.wheight=table(whole_weight.intervalos)
pct=round(100*prop.table(frec.abs.gender))
pie(frec.abs.wheight, main="Toda la muestra",labels=paste(names(frec.abs.wheight),labels=paste(pct,"%")))
pie(frec.abs.wheight, main="Toda la muestra")


weight_F.intervalos=cut(weight_F,breaks=L,right=FALSE,include.lowest=TRUE)
frec.abs.wheight_F=table(weight_F.agrupado)
pie(frec.abs.wheight_F,main="Muesta femenina",labels=paste(names(frec.abs.wheight_F),labels=paste(pct,"%")) )

weight_M.intervalos=cut(weight_M,breaks=L,right=FALSE,include.lowest=TRUE)
frec.abs.wheight_M=table(weight_M.agrupado)
pie(frec.abs.wheight_M, main="Muestra masculina",labels=paste(names(frec.abs.wheight_F),labels=paste(pct,"%")) )


#histiogramas
L=c(0.0,0.3,0.6,0.9,1.2,1.5,1.8,2.1,2.4,2.7)
hist(whole_weight, breaks=L,right=FALSE, main="Toda la muestra", xlab="whole weight(g)", ylab="Frecuencia")
abline(v=mean(whole_weight),lwd=2,lty=3,col="darkred")
abline(v=median(whole_weight),lwd=2)

hist(weight_F, breaks=L,right=FALSE,main="Muestra femenina",xlab="whole weight(g)", ylab="Frecuencia") 
abline(v=mean(weight_F),lwd=2,lty=3,col="darkred")
abline(v=median(weight_F),lwd=2)

hist(weight_M, breaks=L, right=FALSE, main = "Muestra masculina")
abline(v=mean(weight_M),lwd=2,lty=3,col="darkred")
abline(v=median(weight_M,lwd=2))

#diagramas de caja
boxplot(whole_weight, horizontal=TRUE, ylim=c (0,2.7), main="Toda la muestra")
abline(v=mean(whole_weight),lwd=2,lty=3,col="darkred")

boxplot(weight_F,horizontal=TRUE, ylim=c (0,2.7) ,main="Muestra femenina")
abline(v=mean(weight_F),lwd=2,lty=3,col="darkred")

boxplot(weight_M,horizontal=TRUE,ylim=c(0,2.7),main="Muestra masculina")
abline(v=mean(weight_M),lwd=2,lty=3,col="darkred")





