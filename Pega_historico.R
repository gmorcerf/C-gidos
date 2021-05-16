install.packages('crypto')
install.packages('strucchange')
install.packages('reshape2')

library('reshape2')

library('strucchange')
library(reshape2)
library(crypto)


#baixa os dados
#todas as moedas já listadas
nome_moeda<-crypto_list(coin = NULL, start_date = NULL, end_date = NULL,coin_list = NULL)



serie_historico<-crypto_history()

write.table(serie_historico,"C:/Users/georg/Documents/Artigo/Nova pasta/dados_060319.csv",sep=";")
save.image()

#cria tabela linha dia, coluna moeda
fechamento<-dcast(serie_historico,date~slug,value.var="close")
volume<-dcast(serie_historico,date~slug,value.var="volume")
market<-dcast(serie_historico,date~slug,value.var="market")
ratio<-dcast(serie_historico,date~slug,value.var="close_ratio")
spred<-dcast(serie_historico,date~slug,value.var="spread")

fechamento[is.na(fechamento)] <- 0
volume[is.na(volume)] <- 0
market[is.na(market)] <- 0
spred[is.na(spred)] <- 0
fechamento[is.na(fechamento)] <- 0


write.table(fechamento,"C:/Users/georg/Documents/Artigo/Nova pasta/fechamento_030319.csv",sep=";")
write.table(volume,"C:/Users/georg/Documents/Artigo/Nova pasta/volume_030319.csv",sep=";")
write.table(market,"C:/Users/georg/Documents/Artigo/Nova pasta/market_030319.csv",sep=";")
write.table(ratio,"C:/Users/georg/Documents/Artigo/Nova pasta/ratio_030319.csv",sep=";")
write.table(spred,"C:/Users/georg/Documents/Artigo/Nova pasta/spred_030319.csv",sep=";")

###############################################################################################################




########################Gera dia que a moeda teve sua primeira negocia??o ############################
data_moeda<-matrix(0,dim(fechamento)[2],3)
data_moeda<-as.data.frame(Primeira_neg)
colnames(data_moeda)<-c("nome","dia","mes")


for (x in 2 : dim(fechamento)[2])
{
  for (y in 1 : dim(fechamento)[1])
  {
    if ((fechamento[y,x])!=0) 
    {
      data_moeda[x,1]<-colnames(fechamento[x]) #nome
      data_moeda[x,2]<-fechamento[y,1] #dia
      data_moeda[x,3]<-substring(fechamento[y,1],1,7)  #mes
      
      break
    }
    
  }
}


write.table(data_moeda,"C:/Users/georg/Documents/Artigo/Nova pasta/data_primeira_negocia.csv",sep=";")

#evoluação do número de moedas criadas por mês e acumulado
Criacao_moedas_mes<-cbind(table(data_moeda[,3]),cumsum(table(data_moeda[,3])))
colnames(Criacao_moedas_mes)<-c("qdt","qdt_ac")
write.table(Criacao_moedas_mes,"C:/Users/georg/Documents/Artigo/Nova pasta/qdt_criacao_moeda.csv",sep=";")

########################Gera dia que a moeda teve sua primeira negocia??o ############################




########################Gera quantidade de moedas com negociacao por dia ############################

moeda_dia<-matrix(0,dim(fechamento)[1],4)
moeda_dia<-data.frame(moeda_dia)
colnames(moeda_dia)<-c("qdt","qdt_0,01","data","mes")



  
 for (x in 2 : dim(fechamento)[1])
 {
  moeda_dia[x,1]<-length(which(fechamento[x,2:dim(fechamento)[2]]!=0))
  moeda_dia[x,2]<-length(which(fechamento[x,2:dim(fechamento)[2]]>=0.01))
  moeda_dia[x,3]<-fechamento[x,1]
  moeda_dia[x,4] <-substring(fechamento[x,1],1,7)
}


#moeda_dia[,2]<-as.Date(moeda_dia[,3])

plot(moeda_dia[,1],moeda_dia[,2],type="l")
  
write.table(moeda_dia,"C:/Users/georg/Documents/Artigo/Nova pasta/qdt_moedas_ativ_dia.csv",sep=";")
########################Gera quantidade de moedas com negocia por dia ############################


########################Gera valor m?dio do valor de fachamento no dia ############################

#retira valores menores que 0.01, pois esses valores são definidos como sem negociação
fechamento0<-fechamento[,-1]
fechamento0[fechamento0<0.01]<-NA

val_med<-matrix(0,dim(fechamento)[1],4)
val_med<-data.frame(val_med)


val_med<-cbind(substring(fechamento[,1],1,10),substring(fechamento[,1],1,7),rowMeans(fechamento0, na.rm = TRUE),rowMeans(fechamento[,-1]))
colnames(val_med)<-c("data","mes","valor_M_0.01","valor_Total")

val_med[,2]<-as.Date(val_med[,2])
plot(val_med[,2],type="l")

write.table(val_med,"C:/Users/georg/Documents/Artigo/Nova pasta/val_med_moeda.csv",sep=";")

########################Gera valor m?dio do valor de fachamento no dia ############################



########################quantidade de dias com negociação por moeda############################

#retira valores menores que 0.01, pois esses valores são definidos como sem negociação

qdt_dais_nego<-matrix(0,dim(fechamento)[1],3)
qdt_dais_nego<-data.frame(qdt_dais_nego)
colnames(qdt_dais_nego)<-c("qdt","qdt_0,01","moeda")

for (x in 2 : dim(fechamento)[2])
{
  qdt_dais_nego[x,1]<-length(which(fechamento[1:dim(fechamento)[1],x]!=0))
  qdt_dais_nego[x,2]<-length(which(fechamento[1:dim(fechamento)[1],x]>=0.01))
  qdt_dais_nego[x,3] <-colnames(fechamento[x])
}

#moeda_dia[,2]<-as.Da

write.table(qdt_dais_nego,"C:/Users/georg/Documents/Artigo/Nova pasta/qdt_dais_nego.csv",sep=";")

########################quantidade de dias com negociação por moeda############################



