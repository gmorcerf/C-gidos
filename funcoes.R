#install.packages("igraph")
#install.packages("network")
#install.packages("sna")
#install.packages("ndtv")
#install.packages("signnet")
#install.packages("dgof")

#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("ggplot2")
#install.packages("e1071")
#install.packages("ggraph")

#install.packages("GGally")
#install.packages("reshape2")

install.packages("dendextend")
install.packages("RColorBrewer")

install.packages("binancer")
library(binancer)
library(dendextend)

library(tidyverse)
library(RColorBrewer)

library("netbio")
library("GGally")
library("ggraph")

library("igraph")
library("network")
library("sna")
library("ndtv")
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(lmtest)
library(forecast)
library(lubridate)
library(dplyr)
library("ggpubr")
library("ggplot2")
library(e1071)
library(dgof)
  library(signnet)

library(reshape2)
library(ggplot2)

dfcor_13<-as.data.frame(cor_13)
df_cor_13<-melt(cor_13)

ggplot(data = df_cor_13, aes(x=Var1, y=Var2, fill=value))+   geom_tile()


dfcor_14<-as.data.frame(cor_14)
df_cor_14<-melt(cor_14)

ggplot(data = df_cor_14, aes(x=Var1, y=Var2, fill=value))+   geom_tile()


#############################################################

#função verifica os dias sem negociaçã e replica o valor do dia anterior
ajust_dia<-function(base_in,base_out)
{
  base_out<-base_in
  for (x in 1 : dim(base_in)[2])
  {
    i<-0
    for (y in 2 : dim(base_in)[1])
    {
      if (base_in[y,x]==0 && base_out[y-1,x]!=0)
      {
        i<-i+1
        base_out[y,x]<-base_in[y-i,x]
      } 
      
  }

  }
  return(base_out)
}


#######################################################################################################

#função gera o log do retorno das negociaçoes

log_retrono<-function(base_in,base_out)
{
  bse_out<-base_in
for (x in 1 : dim(base_in)[2])
{
  for (y in 2 : dim(base_in)[1])
  {
    base_out[y,x]<-log(base_in[y,x])-log(base_in[y-1,x])
  }
}
return(base_out)
}


#######################################################################################################

#Função que cria correlação média

media_matri<-function(base_in,media_cor)
{
i<-0
acum<-0
for (x in 1 : dim(base_in)[2])
{
  for (y in 1 : dim(base_in)[1])
  {
    if (y>x)
    {
      i<-i+1  
      acum<-base_in[x,y]+acum
    }
  }
 
}
media_cor<-acum/i
return(media_cor) 
}



#######################################################################################################
  #Converte a matriz de correlação em vetor

matrix_vet<-function(base_in,base_out)
{
  base_out<-rep(0,(dim(base_in)[1]*dim(base_in)[1])/2-14)


i<-0
for (x in 1 : dim(base_in)[2])
{
  for (y in 1 : dim(base_in)[1])
  {
    if (y>x)
    {
      i<-i+1  
      base_out[i]<-base_in[x,y]
    }
  }
  
}
return(base_out)
}




####################################################################################
#################################################grafo com sinal #######################################
 
#antes tem q rodar media_matri para pegar a média da matriz triangular
graf_signed<-function(base_in,base_out,media_cor)
{
base_out<-base_in
val_med_abs<-media_cor
n_val_med_abs<-val_med_abs*-1

for ( i in 1 : dim(base_in)[1])
{
  for ( j in 1 : dim(base_in)[2])
  {
    if(base_in[i,j]>=val_med_abs)
    {
      base_out[i,j]<- 1
    }
        else if (base_in[i,j]<= n_val_med_abs)
        {
          base_out[i,j]<- -1
        }
    else if (base_in[i,j]>= n_val_med_abs &  base_in[i,j]<= val_med_abs)
{base_out[i,j]<-0
  }
  
}

}
return(base_out)
}





#Função que cria pesinal da matrix de grafo sem considerar valores 0

pega_sinal<-function(base_in,base_out)
{
  base_out<-rep(0,(dim(base_in)[2]))
  base_out<-0
  i<-0
  for (x in 1 : dim(base_in)[2])
  {
    for (y in 1 : dim(base_in)[1])
    {
      if (y>x && base_in[x,y]!=0)
      {
        i<-i+1  
        base_out[i]<-base_in[x,y]
      }
    }
    
  }
  base_out1<-base_out[1:i]
  return(base_out1)
}


