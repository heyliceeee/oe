#A-0 B-1   #amostra de 21 testes

condutor=c(1, 0, 0, 0, 0,
       1, 0, 1, 0, 0,
       1, 1, 1, 0, 1,
       0, 0, 1, 0, 1, 
       0) #qualitativa  nominal (binominal)

peso=c(2035, 1730, 1180, 1530, 1750,
       1940, 1460, 1960, 1850, 1533,
       1760, 1650, 2050, 1710, 1897,
       1380, 1490, 1820, 1510, 1950,
       1570) #quantitativa continua

distancia=c(138.9, 181.8, 243.9, 204.1, 178.6,
       163.9, 217.4, 163.9, 175.4, 198.2,
       178.6, 188.7, 137.0, 185.2, 166.7,
       212.8, 221.7, 172.4, 204.1, 151.5,
       196.1) #quantitativa continua

consumo=c(11, 6.3, 5.1, 6.3, 6.5,
       8.1, 5.3, 7.9, 7.3, 7.9,
       6.7, 6.1, 9.8, 5.9, 8.2,
       5.9, 5.2, 6.1, 5.4, 7.5, 
       5.9) #quantitativa continua

avaliacao=c(4, 2, 1, 2, 3,
       4, 1, 4, 3, 3,
       3, 2, 4, 3, 4,
       2, 1, 3, 1, 4,
       2) #qualitativa ordinal


#criar tabela  
table=cbind(condutor, peso, distancia, consumo, avaliacao)
table #mostra tabela
summary(table)

#tabelas freq aboslutas e relativas
table(condutor) #nominal (circular, barras)
table(peso) #continua (histograma, diagrama de extremos e quartis)
table(distancia) #continua
table(consumo) #continua
table(avaliacao) #ordinal (barras, diagrama de extremos e quartis)

#Tabelas
f_g<-table(condutor) #fa do condutor
f_g #n de elementos de cada grupo (fa do condutor)
fr_g<-prop.table(f_g) #tabelas de forma proporcional da fr da fa do condutor 
fr_g #fr por condutor

#circular para freq absoluta ou freq relativa
#Representacoes Graficas
nomes_c<-c("condutor A","condutor B") #legendas
cores<-c("pink","skyblue") #cores do grafico
rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ") #dados (legendas, n. elementos)
pie(f_g, main="Numero de Testes realizados por condutor",labels=rotulo,col=cores) #grafico circular

nomes_c<-c("condutor A","condutor B") #legendas
cores<-c("pink","skyblue") #cores do grafico
rotulo<-paste(nomes_c,"(",paste(round(100*fr_g),"%"),")",sep=" ") #dados (legendas, n. elementos)
pie(fr_g, main="Percentagem de Testes realizados por condutor",labels=rotulo,col=cores) #grafico circular

#Grafico Barras (freq absoluta ou freq relativa) (vari�veis nominal e ordenal; Nao serve para var. cont�nuas!!!!)
barplot(f_g,main="Numero de Testes realizados por condutor",xlab="condutor",ylab="n de testes",col=c("pink", "skyblue"), ylim=c(0,15)) #ylim(limites de y) #fa de cada condutor
barplot(fr_g,main="Percentagem de Testes por condutor",xlab="condutor",ylab="n de testes",col=c("pink", "skyblue"), ylim=c(0,1)) #ylim(limites de y) #fr de cada condutor
#text(locator(n=2), paste(round(fr_g, 2))) #introduzir texto no grafico


#Histograma
h<-hist(distancia,main="Distribuicao dos testes por distancia, com 10 litros de gasoleo",xlab="distancia",ylab="Numero de testes", col="skyblue",xlim=c(0,300),ylim=c(0,10))
h
summary(distancia)

h<-hist(peso,main="Distribuicao dos testes por peso",xlab="peso",ylab="Numero de testes",col="skyblue",xlim=c(0,2500),ylim=c(0,10))
h
summary(peso)

#freq_rel=h$counts/21 #freq relativa
#text(locator(n=7), paste(round(freq_rel, 2))) #introduzir texto no grafico


#caixa de bigodes multiplo (comparar 2 v�riaveis (continua - nominal ou ordinal))
boxplot(distancia ~ condutor, main = "Comparacao da distancia por condutor", ylab="distancia em kms", xlab="", 
names=c("condutor A","condutor B"),col=c("pink","blue"))
IQR(distancia) #da intervalo interquartil
tapply(distancia,condutor,summary)

boxplot(peso ~ condutor, main = "Comparacao do peso por condutor", ylab="peso em kg", 
xlab="", names=c("condutor A","condutor B"),col=c("pink","blue"))
IQR(peso)
tapply(peso,condutor,summary)

boxplot(consumo ~ condutor, main = "Comparacao do consumo por condutor", ylab="consumo medio de litros aos 100km", 
xlab="", names=c("condutor A","condutor B"),col=c("pink","blue"))
IQR(consumo)
tapply(consumo,condutor,summary)


boxplot(distancia ~ avaliacao, main = "Comparacao da distancia por avaliacao", 
ylab="distancia em kms", xlab="", names=c("1","2","3","4"),col=c("pink","blue"))
IQR(distancia)
tapply(distancia,avaliacao,summary)

boxplot(peso ~ avaliacao, main = "Comparacao do peso por avaliacao", 
ylab="peso em kg", xlab="", names=c("1","2","3","4"),col=c("pink","blue"))
IQR(peso)
tapply(peso,avaliacao,summary)

boxplot(consumo ~ avaliacao, main = "Comparacao do consumo por avaliacao", 
ylab="consumo medio de litros aos 100km", xlab="", names=c("1","2","3","4"),col=c("pink","blue"))
IQR(consumo)
tapply(consumo,avaliacao,summary)



#comparar 2 variaveis continuas

#---------------- distancia vai depender do peso do automovel e do condutor ----------------#

# PREVISAO: Regressao linear entre distancia e peso
plot(peso, distancia, pch = 1, cex = 1.3, col = "blue", main = "peso vs distancia", xlab = "peso", ylab = "distancia")
# existe uma relacao de correlacao fraca e negativa entre as variaveis "peso" e "distancia"
cor(peso,distancia) # r= -0.9772903, esta muito longe de 1 por isso existe uma correlacao fraca e negativa


# y = B0 + B1 * X + erro <- regressao linear simples
# peso = B0(=3253.986) + B1(=-8.395) * distancia + erro (se o distancia for nulo(zero), o peso e de 3253kg)
# ^peso = 3253.986 + -8.395 * distancia (por cada km q se acrescenta � distancia, acrescenta -8.395kg no peso)


model = lm(peso ~ distancia) #linear simples #dependente~independente
model #peso=3253.986+-8.395*distancia
abline(model, col="red") #desenhar a reta de regressao linear simples
summary(model) #sumario com as estimativas dos coeficientes

#residuals(erros do modelo do y = ... + erro)
#

#predict(model) #previsao - valores estimados pela reta para cada valor de x dado
# coeficiente de correlacao e coeficiente de determinacao, para a qualidade do ajustamento
cor(distancia,peso) # r= -0.9772903
cor(distancia,peso)^2  # r^2 = 0.9550963  (95,50% da variancia de y e explicada pela variancia de x)


3253.986+-8.395*100 #Estima-se que um automóvel que tivesse distancia de 100km a peso seria 2414.486kg
3253.986+-8.395*200 #Estima-se que um automóvel que tivesse distancia de 300km a peso seria 1574.986kg
(100-3253.986)/-8.395 #375.6982km é o distancia estimado para um automóvel que a peso seria 100kg???


#---------------- consumo vai depender da distancia ----------------#
# PREVISAO: Regressao linear entre consumo e distancia
plot(distancia, consumo, pch = 1, cex = 1.3, col = "blue", main = "Distancia vs Consumo", xlab = "distancia (km)", ylab = "consumo medio (l/100km)")
# existe uma relacao de correlacao moderada e negativa entre as variaveis "distancia" e "consumo"
cor(distancia,consumo) # r= -0.8343691, esta muito longe de 1 por isso existe uma correlacao moderada e negativa

# y = B0 + B1 * X + erro <- regressao linear simples
# distancia = B0(=286.61) + B1(=-14.81) * consumo + erro (se o consumo for nulo(zero), a distancia e de km)
# ^distancia = 286.61 + -14.81 * consumo (por cada litro/100km q se acrescenta ao consumo, acrescenta -14.81km na distancia)

model = lm(distancia ~ consumo) #linear simples #dependente~independente
predict(model)
model #distancia=286.61+-14.81*consumo
abline(model, col="red") #desenhar a reta de regressao linear simples
summary(model) #sumario com as estimativas dos coeficientes

#residuals(erros do modelo do y = ... + erro)
#

#predict(model) #previsao - valores estimados pela reta para cada valor de x dado
# coeficiente de correlacao e coeficiente de determinacao, para a qualidade do ajustamento
cor(consumo,distancia) # r= -0.8343691
cor(consumo,distancia)^2  # r^2 = 0.6961718  (69,61% da variancia de y e explicada pela variancia de x)


286.61+-14.81*5 #Estima-se que um automovel que tivesse consumo de 5 litros/100km a distancia seria 212.56km
286.61+-14.81*8 #Estima-se que um automovel que tivesse consumo de 8 litros/100km  a distancia seria 168.13km
(5-286.61)/-14.81 # 19.01485 litros/100km e o consumo estimado para um automovel que a distancia seria 5km


#---------------- consumo vai depender da peso ----------------#
# PREVISAO: Regressao linear entre consumo e peso
plot(peso, consumo, pch = 1, cex = 1.3, col = "blue", main = "Peso vs Consumo", xlab = "peso (kg)", ylab = "consumo medio (l/100km)")
# existe uma relacao de correlacao moderada e positiva entre as variaveis "peso" e "consumo"
cor(peso,consumo) # r= 0.7866102, esta muito longe de 1 por isso existe uma correlacao moderada e positiva

# y = B0 + B1 * X + erro <- regressao linear simples
# peso = B0(=878.2) + B1(=119.9) * consumo + erro (se o consumo for nulo(zero), o peso e de kg)
# ^peso = 878.2 + 119.9 * consumo (por cada litro/100km q se acrescenta ao consumo, acrescenta 119.9kg no peso)

model = lm(peso ~ consumo) #linear simples #dependente~independente
model #peso=878.2+119.9 * consumo
abline(model, col="red") #desenhar a reta de regressao linear simples
summary(model) #sumario com as estimativas dos coeficientes

#residuals(erros do modelo do y = ... + erro)
#

#predict(model) #previsao - valores estimados pela reta para cada valor de x dado
# coeficiente de correlacao e coeficiente de determinacao, para a qualidade do ajustamento
cor(consumo,peso) # r= 0.7866102
cor(consumo,peso)^2  # r^2 = 0.6187556 (61,87% da variancia de y e explicada pela variancia de x)


878.2+119.9 *5 #Estima-se que um automovel que tivesse consumo de 5 litros/100km o peso seria 1477.7kg
878.2+119.9 *8 #Estima-se que um automovel que tivesse consumo de 8 litros/100km o peso seria 1837.4kg
(5-878.2)/119.9 # -7.282736 litros/100km e o consumo estimado para um automovel que o peso seria 5kg


###### Estat�stica INDUTIVA
#inferencia estatistica


#exercicio
#C�lculo de propor��es:
dbinom (8, 21, 0.36) #P(condutor B = 8 em 21, 0.36) = 17.35% (a amostra pode n ser representativa)


#teste a normalidade
shapiro.test(peso) #w(estatistica de teste) #como p-value(0.6065) > 0.05, nao rejeita
shapiro.test(distancia) #w(estatistica de teste) #como p-value(0.9803) > 0.05, nao rejeita
shapiro.test(consumo) #w(estatistica de teste) #como p-value(0.02075) < 0.05, rejeita



#t-test a media populacional
t.test(peso, 
       alternative="two.sided",
       mu=1700, #media do peso
       conf.level=0.95) 
#nivel de confianca(nivel de significancia(100-95=5%) 
#df(grau de liberdade) 
#pvalue(valor de erro) #interval: ....(1quantil, 2quantil) 
#como 1700 esta dentro de 1702.619(mean of x)..., entao esta certo


t.test(distancia, 
       alternative="two.sided",
       mu=180, #media da distancia
       conf.level=0.90) 
#nivel de confianca #df(grau de liberdade) 
#pvalue(valor de erro) #interval: ....(1quantil, 2quantil) 
#como 180 esta dentro de 184.8048(mean of x)..., entao esta certo


t.test(consumo, 
       alternative="two.sided",
       mu=6.3, #media do consumo
       conf.level=0.90) 
#nivel de confianca #df(grau de liberdade) 
#pvalue(valor de erro) #interval: ....(1quantil, 2quantil) 
#como 6.3 esta dentro de 6.87619(mean of x)..., entao esta certo


#compare medias entre grupos
boxplot(distancia ~ condutor, main = "Comparacao da distancia por condutor", ylab="ordenado em kms", xlab="", names=c("condutor A","condutor B"),col=c("pink","blue"))
tapply(distancia,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria))

t.test(distancia ~ condutor,
       alternative="two.sided", 
       conf.level=0.95) 
#p-value < nivel significancia, ou seja, rejeitamos hipotese nula
#a diferenca entre as medias na populacao e positiva, entao e o grupo 0 (condutor A) com
#maior distancia

t.test(peso ~ condutor,
       alternative="two.sided", 
       conf.level=0.95) 
#p-value < nivel significancia, ou seja, nao rejeitamos hipotese nula
#a diferenca entre as medias na populacao e negativa, entao e o grupo 1 (condutor B) com
#maior peso

t.test(consumo ~ condutor,
       alternative="two.sided", 
       conf.level=0.95) 
#p-value < nivel significancia, ou seja, nao rejeitamos hipotese nula
#a diferenca entre as medias na populacao e negativa, entao e o grupo 1 (condutor B) com
#maior consumo



#Teste � signific�ncias dos coeficientes de regress�o e de determina��o
summary(model)


# Teste � normalidade dos erros
shapiro.test(model$residuals)
t.test(model$residuals, 
       alternative="two.sided",
       mu=0,
       conf.level=0.95) #pvalue e maior que nivel de significancia, ou seja, nao rejeito h0


#duas continuas: regressao linear simples

plot(peso, distancia, pch = 1, cex = 1.3, col = "blue", main = "peso vs distancia", xlab = "peso", ylab = "distancia")

cor(peso,distancia) # r= -0.9772903

model = lm(distancia ~ peso) #linear simples #dependente~independente
predict(model)
model #ver modelo;peso=378.5198+-0.1138*distancia
abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); a distancia vai depender do peso do automovel e do condutor
summary(model) # sumario com as estimativas dos coeficientes, p-value e r-quadrado
#intercept e peso tem o p-value muito proximo de zero
#f-statistic: p-value muito pequeno entao rejeita-se h0

cor(peso,distancia)^2  # r^2 = 0.9550963  (95,50% da variancia de y e explicada pela variancia de x)


summary(model)




378.5198+-0.1138*2000 #se a peso for 2000kg, entao a distancia e 150.9198km