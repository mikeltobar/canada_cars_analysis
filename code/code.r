#Código en R empleado para la realización de la práctica.

#1.Carga de librerías

install.packages("ggplot2")
library("ggplot2")
install.packages("gridExtra")
library("gridExtra")
install.packages("fastDummies)
library("fastDummies")
install.packages("kableExtra2")
library("kableExtra")
install.packages("boruta")
library("Boruta")
install.packages("ggcorrplot")
library("ggcorrplot")
install.packages("pROC")
library("pROC")
install.packages("tinytex")
library("tinytex")
install.packages("webshot")
webshot::install_phantomjs()

#2.Descripción del dataset

#Lectura del archivo de descripción

desc<-("../data_files/Data Description.csv")
names(desc)<-c("Variable", "Descripción del valor")
kable_styling(kable(desc, format='html', caption = "Descripcion valores de CO2 Emissio
ns_Canada"))

#3.Integración y selección

#Carga del juego de datos

co2<-read.csv("../data_files/CO2 Emissions_Canada.csv")
str(co2)

#Transformación a factores

chars<-colnames(co2[sapply(co2,is.character)])
co2[chars]<-lapply(co2[chars],factor)
summary(co2)

#Separación variable "Transmission"

co2$Gears<-as.numeric(gsub("[a-zA-Z]*","",co2$Transmission))
co2$Transmission<-gsub("[0-9]","",co2$Transmission)

#Cambio nombres variables

names(co2)[8:12]<-c("City.L.100km", "Hwy.L.100km", "Comb.L.100km", "Comb.mpg", "CO2.g.
km")

#Obtención muestra

head(co2,5)

#4. Limpieza de datos

#Cuenta de duplicados

nrow(co2[duplicated(co2),])

#Comprobación de vacíos
colSums(is.na(co2))
sum(colSums(co2==0))
sum(colSums(co2==""))

#Eliminación duplicados

co2<-co2[!duplicated(co2),]
nrow(co2[duplicated(co2),])
#Visualización diagramas de caja
nums<-colnames(co2[sapply(co2,is.numeric)])
myplots <- vector('list', length(nums))
for (i in seq(1:length(nums))){
message(i)
myplots[[i]] <- local({
i <- i
h<-ggplot(co2, aes(y=.data[[nums[i]]])) +
geom_boxplot()+scale_fill_brewer(palette="Dark2")+
labs(y=nums[i])
})
}
do.call("grid.arrange", c(myplots, ncol=3))

#5. Análisis de los datos

#División en dos grupos

manual_automatico <- co2[,c("Transmission", "CO2.g.km")]
comparacion_manual <- subset(co2 , Transmission == "M")
comparacion_resto <- subset(co2 , Transmission != "M")
comparacion_manual$manual<-"Manual"
comparacion_resto$manual<-"Resto"

#Comprobación normalidad y homogeneidad varianza-gráfico

nrow(comparacion_manual)
nrow(comparacion_resto)
ggplot(data=comparacion_manual, aes(x=CO2.g.km, fill=manual))+ geom_density(alpha=0.4)
+
ggtitle("Density plot of CO2 emissions on Transmission")+geom_density(data = compara
cion_resto, aes(x=CO2.g.km), alpha=0.4)+
xlab(expression(paste(CO^{2},frac(g,km))))+labs(fill="Transmisión")
#Test de varianzas
testVarianzas <- function(x,y){
var.test(x,y)
}
testVarianzas(comparacion_manual$CO2.g.km, comparacion_resto$CO2.g.km)

#Contraste de hipótesis-valores del intervalo y t.test

qnorm(0.05)
t.test(comparacion_manual$CO2.g.km, comparacion_resto$CO2.g.km,alternative=c("less"))

#Correlaciones-comprobaciones y visualización

results <- dummy_cols(co2, select_columns = c("Transmission", "Fuel.Type"))
results$Fuel.Type<-NULL
nums2<-results[,colnames(results[sapply(results,is.numeric)])]
nums2$Gears<-NULL
source("http://www.sthda.com/upload/rquery_cormat.r")
cormat<-rquery.cormat(nums2, type="flatten", graph = FALSE)
cormat.ordered<-head(cormat$r[order(abs(cormat$r$cor), decreasing = TRUE),],20)
kable_styling(kable(cormat.ordered, format='html', caption = "Correlaciones entre algunas variables"))

#Regresión logística-visualización datos y establecimiento límite

ggplot(data = co2, aes(x=CO2.g.km)) + geom_histogram() +
annotate(geom = "vline",
x = median(co2$CO2.g.km),
xintercept = median(co2$CO2.g.km),
linetype = "dashed")+
annotate(geom = "text",
label = "Median",
x = median(co2$CO2.g.km),
y = 300,
angle = 90,
vjust = 1)+
ggtitle("Histogram of CO2 emissions")+
xlab(expression(paste(CO^{2},frac(g,km))))
summary(co2$CO2.g.km)
co2["co2.g.km.binary"] <- cut(co2$CO2.g.km, breaks = c(0,246,10000), labels = c("0",
"1"))

#Regresión logística - Boruta

co2.boruta<-co2
co2.boruta$Gears<-NULL
co2.boruta$CO2.g.km<-NULL
boruta.co2 <- Boruta(co2.g.km.binary~., data = co2.boruta, doTrace = 2)
print(boruta.co2)
par(mar=c(10,5,5,5)+.1)
plot(boruta.co2, xlab= "", las=3)
co2.boruta$Model<-NULL
glm.co2<- glm(co2.g.km.binary~., family=binomial, data=co2.boruta)
summary(glm.co2)

#6.Representación gráfica

#Matriz correlaciones

corrs <- round(cor(nums2), 2)
ggcorrplot(corrs)

#Bondad del ajuste de la regresión

p1=predict(glm.co2, co2.boruta, type="response")
r1=roc(co2.boruta$co2.g.km.binary,p1, data=co2.boruta)
plot(r1)
