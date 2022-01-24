#variaveis

 teste = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
 
 #condutor = c("B","A","A","A","A","B","A","B","A","A","B","B","B","A","B","A","A","B","A","B","A")
 
 # A - 0 | B - 1
 condutor = c(1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0)
 
 peso = c(2035,1730,1180,1530,1750,1940,1460,1960,1850,1533,
          1760,1650,2050,1710,1897,1380,1490,1820,1510,1950,1570)
 
 distancia = c(138.9,181.8,243.9,204.1,178.6,163.9,217.4,163.9,175.4,198.2,
               178.6,188.7,137.0,185.2,166.7,212.8,221.7,172.4,204.1,151.5,196.1)
 
#Tabelas Freq. Absolutas e Relativas
 
 tabela=cbind(teste, condutor, peso, distancia) 
 
 table(teste) #continua
 table(condutor) #var Nominal - Grupos
 table(peso) #continua
 table(distancia) #continua
 
 # distancia + peso -> continuas -> podem ser usadas em graficos de estremos e quartis e histogramas
 # condutor -> nominal -> grafico circular ou barras 
  
 #Freq. Absoluta de condutor
 f_g<-table(condutor)
 f_g
 
 #Freq. Relativa de condutor
 fr_g<-prop.table(f_g)
 fr_g
 
 # Representaçoes graficas

 #Grafico circular
 pie(f_g)
 
  nomes_c<-c("Condutor A", "Condutor B") #legendas
  cores<-c("purple","skyblue") #cores do grafico
  rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ") #dados (legendas, n. elementos)
  pie(f_g, main="Numero de testes por condutor",labels=rotulo,col=cores) #grafico circular
  
  #Gr�fico circular 3D
  pie3D(f_g,explode = 0.05,main="Numero de testes por condutor",labels=rotulo,col=cores, labelcex = 1.1) #grafico circular
 
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
  
  
 #boxplots
  
  boxplot(peso)
  summary(peso) 
  
  #double boxplot peso Vertical
  boxplot(peso ~ condutor, main = "Comparacao do Peso do veiculo por condutor", ylab="Peso (Kg)", xlab="Condutor", names=c("A","B"),col=c("purple","skyblue"))
  
  #double boxplot peso Horizontal
  boxplot(peso ~ condutor, horizontal = TRUE, main = "Comparacao do Peso do veiculo por condutor", ylab="Peso (Kg)", xlab="Condutor", names=c("A","B"),col=c("purple","skyblue"))
  
  
  #segundo esta amostra o genero feminino tem o ordenado atual menor que dos homens
  IQR(peso) #da intervalo interquartil (no ordenado atual, a diferenca entre o 1 qartil e o 3 quartil sao 798.85)
  tapply(peso,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria); o min da mulher e menos de metade do min do homem; 1 quartil e 3 quartil(50%);)
  
  #double boxplot distancia
  boxplot(distancia ~ condutor, main = "Comparaçao do ordenado atual por genero", ylab="ordenado em euros", xlab="", names=c("A","B"),col=c("pink","blue"))
  #segundo esta amostra o genero feminino tem o ordenado atual menor que dos homens
  IQR(distancia) #da intervalo interquartil (no ordenado atual, a diferenca entre o 1 qartil e o 3 quartil sao 798.85)
  tapply(distancia,condutor,summary)# Para interpretar os valores do boxplot (qt + proximo a media e a mediana estao, mais normais estao (assimetria); o min da mulher e menos de metade do min do homem; 1 quartil e 3 quartil(50%);)
  
  
  ###### Estatistica INDUTIVA    
  # PREVISAO: Regressao linear entre ord_inic e ord_atual
  plot(peso, distancia, pch = 20, col = "purple", main = "Distancia vs Peso", xlab = "Peso", ylab = "Distancia")
  # relacao da var da ord_inic e ord_atual (se existe correlacao forte ou fraca)
  # existe uma relacao de correlacao forte e positiva entre as variaveis "ord_inic" e "ord_atual"
  cor(peso, distancia) # r= 0.9658736 , esta muito proximo de 1 por isso existe uma correlacao forte e positiva
  cor(peso, distancia)^2
  
  model = lm(distancia ~ peso) #linear simples #dependente~independente
  model #ver modelo;ord_atual=384.978+1.035*ord_inicial
  
  abline(model, col="red") #desenhar a reta de regressao linear simples #intercept(ordenada da origem); se o ord_inic se fosse nulo(zero), o ord_atual e de 384??? #ord_inic()
  summary(model) # sumario com as estimativas dos coeficientes, p-value e r-quadrado 
  #intercept(se o modelo pode ou n ser anulado; tvalue; pvalue(rejeitar ou n rejeitar dependendo do valor de alpha; se for menor rejeita-se a hipotese nula; o intercept(=0.00136) tem um significado pq e diferente de zero))
  #ord_inic(se for menor(=0.00000000000209) rejeita-se a hipotese nula)
  
  
  

  
  
  