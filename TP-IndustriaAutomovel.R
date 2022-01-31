install.packages(c("plotrix"))

library(plotrix)

#variaveis

 teste = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
 
 #condutor = c("B","A","A","A","A","B","A","B","A","A","B","B","B","A","B","A","A","B","A","B","A")
 
 # A - 0 | B - 1
 condutor = c(1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0)
 
 peso = c(2035,1730,1180,1530,1750,1940,1460,1960,1850,1533,
          1760,1650,2050,1710,1897,1380,1490,1820,1510,1950,1570)
 
 distancia = c(138.9,181.8,243.9,204.1,178.6,163.9,217.4,163.9,175.4,198.2,
               178.6,188.7,137.0,185.2,166.7,212.8,221.7,172.4,204.1,151.5,196.1)
 
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
 
#Tabelas Freq. Absolutas e Relativas
 
 tabela=cbind(teste, condutor, peso, distancia, consumo, avaliacao) 
 
 tabelaPD=cbind(peso, distancia)
 
 #tabelas freq aboslutas e relativas
 table(condutor) #nominal (circular, barras)
 table(peso) #continua (histograma, diagrama de extremos e quartis)
 table(distancia) #continua
 table(consumo) #continua
 table(avaliacao) #ordinal (barras, diagrama de extremos e quartis)
 
 summary(tabelaPD)
 
 # distancia + peso -> continuas -> podem ser usadas em graficos de estremos e quartis e histogramas
 # condutor -> nominal -> grafico circular ou barras 
  
 #Freq. Absoluta de condutor
 f_g<-table(condutor)
 f_g
 
 #Freq. Relativa de condutor
 fr_g<-prop.table(f_g)
 fr_g
 
  # -------------------------------------------------------------------------------------------- #
 
 # Representaçoes graficas

 #Grafico circular
 pie(f_g)
 
  nomes_c<-c("Condutor A", "Condutor B") #legendas
  cores<-c("purple","skyblue") #cores do grafico
  rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ") #dados (legendas, n. elementos)
  pie(f_g, main="Numero de testes por condutor",labels=rotulo,col=cores) #grafico circular

  nomes_c<-c("condutor A","condutor B") #legendas
  cores<-c("pink","skyblue") #cores do grafico
  rotulo<-paste(nomes_c,"(",paste(round(100*fr_g),"%"),")",sep=" ") #dados (legendas, n. elementos)
  pie(fr_g, main="Percentagem de Testes realizados por condutor",labels=rotulo,col=cores) #grafico circular
  
  #Grafico circular 3D - Numero de testes
  nomes_c<-c("condutor A","condutor B") #legendas
  cores<-c("pink","skyblue") #cores do grafico
  rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ") #dados (legendas, n. elementos)
  pie3D(f_g,explode = 0.05,main="Numero de testes por condutor",labels=rotulo,col=cores, labelcex = 1.1) #grafico circular
  
  #Grafico circular 3D - Percentagem de testes
  nomes_c<-c("condutor A","condutor B") #legendas
  cores<-c("pink","skyblue") #cores do grafico
  rotulo<-paste(nomes_c,"(",paste(round(100*fr_g),"%"),")",sep=" ") #dados (legendas, n. elementos)
  pie3D(fr_g,explode = 0.05,main="Percentagem de Testes realizados por condutor",labels=rotulo,col=cores, labelcex = 1.1) #grafico circular

  # -------------------------------------------------------------------------------------------- #
  
 #Graficos de barras
  
  #barplot(f_g, main="Numero de Individuos por tipo de condutor", col="skyblue") 
  
  #Grafico de barras Numero de Testes por condutor
  barplot(f_g,main="Numero de Testes por condutor",xlab="Condutor", names.arg = c("A","B") ,ylab="N. de Testes",col=c("purple","skyblue"), ylim=c(0,21), xlim=c(0,4)) #ylim(limites de y) #fr de cada condutor
  legend("topright", legend = c("Condutor A", "Condutor B"), fill = c("purple","skyblue"), bty = "n")
  text(locator(n=2), paste(round(f_g,2)))
  
  #Grafico de barras Percentagem de Testes por condutor
  barplot(fr_g,main="Pertagem de Testes por condutor",xlab="Condutor",ylab="Percentagem de Testes", names.arg = c("A","B"), col=c("purple","skyblue"), ylim=c(0,1), xlim=c(0,4)) #ylim(limites de y) #fr de cada condutor
  legend("topright", legend = c("Condutor A", "Condutor B"), fill = c("purple","skyblue"), bty = "n")
  text(locator(n=2), paste(round(fr_g,2)))
  
  
  #prop.table(f_g)
  
  # -------------------------------------------------------------------------------------------- #
  
 #histograma
  
  #Numero de testes por Peso
  hp=hist(peso, main="Numero de Testes por Peso", xlab="Peso (Kg)",ylab="N. de Testes" ,col = "skyblue", ylim = c(0,8))
  legend("topright", legend = c("Testes por Peso"), fill = c("skyblue"), bty = "n")
  summary(peso)  
  freq_abs=hp$counts
  text(locator(6), paste(round(freq_abs)))
 
  #Numero de testes por Distancia
  hd=hist(distancia,main="Numero de Testes por Distancia", xlab="Distancia (Km)",ylab="N. de Testes",col = "skyblue", ylim = c(0,10))
  legend("topright", legend = c("Testes por Distancia"), fill = c("skyblue"), bty = "n")
  summary(distancia)
  freq_rel=hd$counts
  text(locator(n=7), paste(round(freq_rel)))
  
  # -------------------------------------------------------------------------------------------- #
  
 #boxplots
  
  #Boxplot Peso + Dif Inter-quartis
  boxplot(peso, main = "Distribuicao do Peso", ylab="Peso (Kg)",col=c("mediumspringgreen"))
    summary(peso) 
  IQR(peso) #da intervalo interquartil (no peso, a diferenca entre o 1 qartil e o 3 quartil sao 367)
  
  #Boxplot Distancia + Dif Inter-quartis
  boxplot(distancia, main = "Distribuicao da distancia", ylab="Distancia (Km)",col=c("lightgoldenrod1"))
  summary(distancia) 
  IQR(distancia) #da intervalo interquartil (na distancia, a diferenca entre o 1 qartil e o 3 quartil sao 37.4)
  
  #Boxplot Peso por condutor + Summary
  boxplot(peso ~ condutor ,main = "Comparacao do Peso do veiculo por condutor", ylab="Peso (Kg)", xlab="Condutor", names=c("A","B"),
  col=c("purple","skyblue"), xlim=c(0,4))
  legend("topright", legend = c("Condutor A","Condutor B"), fill = c("purple","skyblue"), bty = "n")
  
  tapply(peso,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria); o min da mulher e menos de metade do min do homem; 1 quartil e 3 quartil(50%);)
  
  #double boxplot distancia
  boxplot(distancia ~ condutor, main = "Comparacao da Distancia por condutor", ylab="Distancia (Km)", xlab="Condutor", names=c("A","B"),
  col=c("purple","skyblue"))
  #segundo esta amostra o genero feminino tem o ordenado atual menor que dos homens
  legend("topright", legend = c("Condutor A","Condutor B"), fill = c("purple","skyblue"), bty = "n")
  IQR(distancia) #da intervalo interquartil (no ordenado atual, a diferenca entre o 1 qartil e o 3 quartil sao 798.85)
  tapply(distancia,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao 
  #(assimetria); o min da mulher e menos de metade do min do homem; 1 quartil e 3 quartil(50%);)
  
  #double boxplot consumo
  boxplot(consumo ~ condutor, main = "Comparacao do consumo por condutor", ylab="consumo medio de litros aos 100km", 
  xlab="Condutor", names=c("condutor A","condutor B"),col=c("purple","skyblue"))
  legend("topright", legend = c("Condutor A","Condutor B"), fill = c("purple","skyblue"), bty = "n")
  IQR(consumo)
  tapply(consumo,condutor,summary)
  
  #double boxplot distancia - avaliacao
  boxplot(distancia ~ avaliacao, main = "Comparacao da distancia por avaliacao", ylab="Distancia (Km)", xlab="Avaliacao", 
  names=c("1","2","3","4"),col=c("purple","skyblue","darkseagreen","coral"))
  legend("topright", legend = c("1 - desempenho muito bom", "2 - desempenho bom", "3 - desempenho fraco","4 - desempenho muito fraco"),
  fill = c("purple","skyblue","darkseagreen","coral"), bty = "n")
  IQR(distancia)
  tapply(distancia,avaliacao,summary)
  
  #double boxplot peso - avaliacao
  boxplot(peso ~ avaliacao, main = "Comparacao do peso por avaliacao", ylab="Peso (Kg)", xlab="Avaliacao", names=c("1","2","3","4"),
  col=c("purple","skyblue","darkseagreen","coral"))
  legend("topleft", legend = c("1 - desempenho muito bom", "2 - desempenho bom", "3 - desempenho fraco","4 - desempenho muito fraco"),
   fill = c("purple","skyblue","darkseagreen","coral"), bty = "n")
  IQR(peso)
  tapply(peso,avaliacao,summary)
  
  #double boxplot consumo - avaliacao
  boxplot(consumo ~ avaliacao, main = "Comparacao do consumo por avaliacao", ylab="Consumo Medio de Litros aos 100km", xlab="Avaliacao",
   names=c("1","2","3","4"),col=c("purple","skyblue","darkseagreen","coral"))
  legend("topleft", legend = c("1 - desempenho muito bom", "2 - desempenho bom", "3 - desempenho fraco","4 - desempenho muito fraco"),
   fill = c("purple","skyblue","darkseagreen","coral"), bty = "n")
  IQR(consumo)
  tapply(consumo,avaliacao,summary)
  
  # -------------------------------------------------------------------------------------------- #
  
  ###### Estatistica INDUTIVA    
  # PREVISAO: Regressao linear entre peso e distancia
  
  plot(peso,distancia , pch = 19, col = "lightslateblue", main = "Distancia vs Peso",
   xlab = "Peso (Kg)", ylab = "Distancia (Km)")
  cor(peso, distancia) # r= -0.9772903
  cor(peso, distancia)^2
  
  model = lm(distancia ~ peso) #linear simples #dependente~independente # dependente - Y | Independente - X
  model #ver modelo;distancia=378.5198-0.1138*peso
  
  abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); se o peso se fosse nulo(zero), a distancia e de 378??? #peso()
  summary(model) # sumario com as estimativas dos coeficientes, p-value e r-quadrado 
  #intercept(se o modelo pode ou n ser anulado; tvalue; pvalue(rejeitar ou n rejeitar dependendo do valor de alpha; se for menor rejeita-se a hipotese nula; o intercept(=0.00136) tem um significado pq e diferente de zero))
  #peso(se for menor(=0.00000000000209) rejeita-se a hipotese nula)
  
  #Residuals -> Erros Distancia de cada um dos pontos � reta
  
  #Coeficientes -> beta 0 378 Beta 1 -0.11378 * peso
  
  #estrelas -> tem a ver com os erros 
  
  #
  
  378.5198-0.1138*2000
  
  
  plot(distancia,consumo , pch = 19, col = "lightslateblue", main = "Distancia vs Consumo", 
  xlab = "Distancia (Km)",ylab = "Consumo Medio (l/100Km)")
  cor(distancia, consumo) # r= -0.8343691
  cor(distancia, consumo)^2 # r^2 = 0.6961718
  

  model = lm(consumo ~ distancia) #linear simples #dependente~independente # dependente - Y | Independente - X
  model 
  
  abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); se o peso se fosse nulo(zero), a distancia e de 378??? #peso()
  summary(model)
  
  
  
  plot(peso,consumo , pch = 19, col = "lightslateblue", main = "Peso vs Consumo",
  xlab = "Peso (Kg)", ylab = "Consumo Medio (l/100Km)")
  cor(peso, consumo) # r= -0.8343691
  cor(peso, consumo)^2 # r^2 = 0.6961718
  
  
  model = lm(consumo ~ peso) #linear simples #dependente~independente # dependente - Y | Independente - X
  model 
  
  abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); se o peso se fosse nulo(zero), a distancia e de 378??? #peso()
  summary(model)
  
  