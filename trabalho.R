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
f_g<-table(condutor) #frequencia absoluta do condutor
f_g #n de elementos de cada grupo (frequencia absoluta do condutor)
fr_g<-prop.table(f_g) #tabelas de forma proporcional da frequencia relativa da frequencia absoluta do condutor 
fr_g #frequencia relativa por condutor (condutor A -> 57%; condutor B -> 42%)

#circular para freq absoluta ou freq relativa
#Representacoes Graficas
nomes_c<-c("condutor A","condutor B") #legendas
cores<-c("pink","skyblue") #cores do grafico
rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ") #dados (legendas, n. elementos)
pie(f_g, main="Numero de Testes realizados por condutor",labels=rotulo,col=cores) #grafico circular

nomes_c<-c("condutor A","condutor B") #legendas
cores<-c("pink","skyblue") #cores do grafico
rotulo<-paste(nomes_c,"(",paste(fr_g),")",sep=" ") #dados (legendas, n. elementos)
pie(fr_g, main="Percentagem de Testes realizados por condutor",labels=rotulo,col=cores) #grafico circular

#Grafico Barras (freq absoluta ou freq relativa) (vari�veis nominal e ordenal; Nao serve para var. cont�nuas!!!!)
barplot(f_g,main="Numero de Testes realizados por condutor",xlab="condutor",ylab="n de testes",col=c("pink", "skyblue"), ylim=c(0,15)) #ylim(limites de y) #fa de cada condutor
barplot(fr_g,main="Percentagem de Testes por condutor",xlab="condutor",ylab="n de testes",col=c("pink", "skyblue"), ylim=c(0,1)) #ylim(limites de y) #fr de cada condutor
#text(locator(n=2), paste(round(fr_g, 2))) #introduzir texto no grafico


#Histograma
h<-hist(distancia,main="Distribuicao dos testes por distancia, com 10 litros de gasoleo",xlab="distancia",ylab="Numero de testes", col="skyblue",xlim=c(0,300),ylim=c(0,10))
h
summary(distancia)

h<-hist(peso,main="Distribuicao dos testes por peso",xlab="peso",ylab="Numero de testes", col="skyblue",xlim=c(0,2500),ylim=c(0,10))
h
summary(peso)

#freq_rel=h$counts/21 #freq relativa
#text(locator(n=7), paste(round(freq_rel, 2))) #introduzir texto no grafico


#caixa de bigodes multiplo (comparar 2 v�riaveis (continua - nominal ou ordinal))
boxplot(distancia ~ condutor, main = "Comparacao da distancia por condutor", ylab="ordenado em kms", xlab="", names=c("condutor A","condutor B"),col=c("pink","blue"))
IQR(distancia) #da intervalo interquartil (na distancia, a diferenca entre o 1 qartil e o 3 quartil sao 37.4)
tapply(distancia,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria))


#comparar 2 vari�veis continuas

#---------------- distancia vai depender do peso do automovel e do condutor ----------------#

# PREVISAO: Regressao linear entre distancia e peso
plot(peso, distancia, pch = 1, cex = 1.3, col = "blue", main = "peso vs distancia", xlab = "peso", ylab = "distancia")
# existe uma relacao de correlacao fraca e negativa entre as variaveis "peso" e "distancia"
cor(peso,distancia) # r= -0.9772903, esta muito longe de 1 por isso existe uma correlacao fraca e negativa


# y = B0 + B1 * X + erro <- regressao linear simples
# peso = B0(=3253.986) + B1(=-8.395) * distancia + erro (se o distancia for nulo(zero), o peso e de 3253kg)
# ^peso = 3253.986 + -8.395 * distancia (por cada km q se acrescenta � distancia, acrescenta -8.395kg no peso)


model = lm(peso ~ distancia) #linear simples #dependente~independente
model #ver modelo;peso=3253.986+-8.395*distancia
abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); a distancia vai depender do peso do automovel e do condutor
summary(model) # sumario com as estimativas dos coeficientes, p-value e r-quadrado

#residuals(erros do modelo do y = ... + erro)
#

#predict(model) #previsao - valores estimados pela reta para cada valor de x dado
# coeficiente de correlacao e coeficiente de determinacao, para a qualidade do ajustamento
cor(peso,distancia) # r= -0.9772903
cor(peso,distancia)^2  # r^2 = 0.9550963  (95,50% da variancia de y e explicada pela variancia de x)


###### Estat�stica INDUTIVA
#inferencia estatistica


#exercicio
#calculo de proporcoes
dbinom(8, 19, 0.49) #P(M=8 em 19, 0.49) = 15.25% (a amostra pode n ser representativa)


#teste a normalidade
shapiro.test(peso) #w(estatistica de teste) #como p-value(0.6065) > 0.05, nao rejeita


#t-test a media populacional
t.test(peso, 
       alternative="two.sided",
       mu=1703, #media do peso
       conf.level=0.95) #nivel de confianca(nivel de significancia(100-95=5%) #df(grau de liberdade) #pvalue(valor de erro) #interval: ....(1quantil, 2quantil) #como 1703 esta fora de 1702.619(mean of x)..., entao esta errado


t.test(distancia, 
       alternative="two.sided",
       mu=184.8, #media da distancia
       conf.level=0.95) #nivel de confianca #df(grau de liberdade) #pvalue(valor de erro) #interval: ....(1quantil, 2quantil) #como 184.8 esta fora de 184.8048(mean of x)..., entao esta errado



#compare medias entre grupos
boxplot(distancia ~ condutor, main = "Comparacao da distancia por condutor", ylab="ordenado em kms", xlab="", names=c("condutor A","condutor B"),col=c("pink","blue"))
tapply(distancia,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria))

t.test(distancia ~ condutor,
       alternative="two.sided", 
       conf.level=0.95) #p-value < nivel significancia, ou seja, rejeitamos hipotese nula
                        #a diferenca entre as medias na populacao e positiva, entao e o grupo 0 (condutor A) com maior distancia


#duas continuas: regressao linear simples

plot(peso, distancia, pch = 1, cex = 1.3, col = "blue", main = "peso vs distancia", xlab = "peso", ylab = "distancia")

cor(peso,distancia) # r= -0.9772903

model = lm(distancia ~ peso) #linear simples #dependente~independente
model #ver modelo;peso=378.5198+-0.1138*distancia
abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); a distancia vai depender do peso do automovel e do condutor
summary(model) # sumario com as estimativas dos coeficientes, p-value e r-quadrado
#intercept e peso tem o p-value muito proximo de zero
#f-statistic: p-value muito pequeno entao rejeita-se h0

cor(peso,distancia)^2  # r^2 = 0.9550963  (95,50% da variancia de y e explicada pela variancia de x)


summary(model)
shapiro.test(model$residuals)
t.test(model$residuals, 
       alternative="two.sided",
       mu=0,
       conf.level=0.95) #pvalue e maior que nivel de significancia, ou seja, nao rejeito h0


378.5198+-0.1138*2000 #se a peso for 2000kg, entao a distancia e 150.9198km