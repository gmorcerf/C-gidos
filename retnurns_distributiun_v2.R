

fechamento<-read.csv2("C:/Users/georg/Documents/Artigo/Nova pasta/fechamento_030319_v1.csv",dec=".")
#transforma em serie temporal
#fechamento_ts<-ts(fechamento1,start=c(2014,04,28), end=c(2019,05,03), frequency = 365)

#formata data
fechamento$date<-as.Date(fechamento$date, format = "%d /%m /%Y")
#fechamento<-fechamento[is.na(fechamento)] <- 0;


#Quebra por ano
fecha_13<-filter(fechamento,year(date)==2013)

#filtra moedas que tem no mínimo 6 meses)
fecha_13_op<-fecha_13[65:dim(fecha_13)[1],c("bitbar","bitcoin","digitalcoin","feathercoin","freicoin","goldcoin",
"ixcoin","litecoin","mincoin","namecoin","novacoin","peercoin","terracoin","worldcoin")]




#Substui dia sem negociação pelo dia anterior por dia anterior
fecha_13_op_ajus<-ajust_dia(fecha_13_op,fecha_13_op_ajus)



#calcula o log do retorno 
fecha_13_return<-fecha_13_op_ajus
fecha_13_return<-log_retrono(fecha_13_op_ajus,fecha_13_return)

#calculando a correlação
cor_13<-cor(fecha_13_return)


##média matriz triagula
media_cor_13<-media_matri(cor_13,media_cor_13)


#Cria matriz 0 e 1 para ografo
#Se correlação for mairo que a méida 1 se não 0
graf_cor_13<-cor_13

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2013<-graf_signed(graf_cor_13,cor_13,media_cor_13)



#convertendo a matriz em vetor
vet_cor_13<-matrix_vet(cor_13,vet_cor_13)
###faz graficos com e sem outliers

box_13<-boxplot(vet_cor_13)

#exclui os valores abaixo do limite inferios e acima do limite superior 
vet_cor_13_so<-vet_cor_13[vet_cor_13>=box_13$stats[1] & vet_cor_13<=box_13$stats[5]]


media_cor_13_so <- mean(vet_cor_13_so)

par(mfrow=c(2,2))
hist(vet_cor_13)
plot(density(vet_cor_13))

histnrow(df[df$x<0,])
plot(density(vet_cor_13_so))




#write.csv2(fecha_13,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2013.csv")


#write.csv2(fecha_13_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_13_op.csv")
#write.csv2(fecha_13_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_13_op_ajus.csv")
#write.csv2(fecha_13_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_13_return.csv")

#write.csv2(cor_13,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_13.csv")
#write.csv2(vet_cor_13_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_13_so.csv")
#write.csv2(vet_cor_13,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_13.csv")
#write.csv2(signed_2013,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2013.csv")




######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################


fecha_14<-filter(fechamento,year(date)==2014)

#filtra moedas que tem no mínimo 6 meses)
fecha_14_op<-fecha_14[,c("X42.coin","anoncoin","argentum","betacoin","bitbar","bitcoin","casinocoin","datacoin","deutsche.emark","diamond","digitalcoin","dogecoin","feathercoin","fedoracoin","flo","freicoin","goldcoin","infinitecoin","ixcoin","joulecoin","litecoin","megacoin","mooncoin","namecoin","novacoin","nxt","omni","peercoin","primecoin","quark","ripple","sexcoin","tagcoin","terracoin","unobtanium","worldcoin","zetacoin")]

#Substui dia sem negociação pelo dia anterior por dia anterior
  
fecha_14_op_ajus<-ajust_dia(fecha_14_op,fecha_14_op_ajus)

#log do retorno
fecha_14_op_ajus->fecha_14_return
fecha_14_return<-log_retrono(fecha_14_op_ajus,fecha_14_return)


#substitui NA e inf por 0

fecha_14_return[sapply(fecha_14_return, is.infinite)] <- 0
fecha_14_return[is.na(fecha_14_return)] <- 0

cor_14<-cor(fecha_14_return)

media_cor_14<-media_matri(cor_14,media_cor_14)

#Função que cria correlação média
graf_cor_14<-cor_14

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2014<-graf_signed(graf_cor_14,cor_14,media_cor_14)


#Converte a matriz de correlação em vetor

vet_cor_14<-matrix_vet(cor_14,vet_cor_14)


###faz graficos com e sem outliers

box_14<-boxplot(vet_cor_14)
min(box_14$out)
vet_cor_14_so<-vet_cor_14[vet_cor_14>=box_14$stats[1] & vet_cor_14<=box_14$stats[5]]

media_cor_14_so <- mean(vet_cor_14_so)

par(mfrow=c(2,2))
hist(vet_cor_14)
plot(density(vet_cor_14))

hist(vet_cor_14_so)
plot(density(vet_cor_14_so))


#write.csv2(fecha_14,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2014.csv")

#write.csv2(fecha_14_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_14_op.csv")
#write.csv2(fecha_14_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_14_op_ajus.csv")
#write.csv2(fecha_14_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_14_return.csv")

#write.csv2(cor_14,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_14.csv")
#write.csv2(signed_2014,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2014.csv")
#write.csv2(vet_cor_14_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_14_so.csv")
#write.csv2(vet_cor_14,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_14.csv")


######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################


fecha_15<-filter(fechamento,year(date)==2015)

#filtra moedas que tem no mínimo 6 meses)
fecha_15_op<-fecha_15[,c("X42.coin","acoin","anoncoin","argentum","artbyte","auroracoin","belacoin","betacoin","bitbar","bitbay","bitcny","bitcoin","bitcoin.scrypt","bitmark","bitshares","bitstar","bitswift","blackcoin","blakecoin","blocknet","bluecoin","boolberry","boostcoin","bunnycoin","burst","bytecoin.bcn","canada.ecoin","cannabiscoin","cashcoin","casinocoin","clams","cloakcoin","coin2.1","counterparty","cryptonite","curecoin","dash","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","dimecoin","dnotes","dogecoin","dopecoin","e.gulden","eccoin","einsteinium","emerald","energycoin","exclusivecoin","faircoin","feathercoin","fedoracoin","flo","foldingcoin","freicoin","gamecredits","gcn.coin","globalboost.y","goldcoin","groestlcoin","gulden","huntercoin","hyperstake","infinitecoin","iocoin","ixcoin","joincoin","joulecoin","litecoin","magi","maidsafecoin","maxcoin","megacoin","mincoin","mintcoin","monacoin","monero","monetaryunit","mooncoin","myriad","namecoin","nav.coin","neoscoin","newyorkcoin","novacoin","nubits","nushares","nxt","nyancoin","okcash","omni","opal","orbitcoin","pandacoin.pnd","paycoin2","peercoin","pesetacoin","photon","piggycoin","pinkcoin","potcoin","prime.xi","primecoin","quark","quotient","rabbitcoin","reddcoin","rimbit","ripple","rubycoin","securecoin","sexcoin","smartcoin","solarcoin","spreadcoin","startcoin","stealth","stellar","storjcoin.x","supercoin","syscoin","tagcoin","tekcoin","terracoin","teslacoin","titcoin","tittiecoin","trollcoin","ubiq","ultracoin","uniform.fiscal.object","unobtanium","verge","vericoin","vertcoin","viacoin","virtacoin","whitecoin","worldcoin","zeitcoin","zetacoin")]

#Substui dia sem negociação pelo dia anterior por dia anterior
fecha_15_op_ajus<-ajust_dia(fecha_15_op,fecha_15_op_ajus)


fecha_15_return<-fecha_15_op_ajus
#função gera o log do retorno das negociaçoes
fecha_15_return<-log_retrono(fecha_15_op_ajus,fecha_15_return)

#substitui NA e inf por 0

fecha_15_return[sapply(fecha_15_return, is.infinite)] <- 0
fecha_15_return[is.na(fecha_15_return)] <- 0


cor_15<-cor(fecha_15_return)


#Função que cria correlação média
media_cor_15<-media_matri(cor_15,media_cor_15)

graf_cor_15<-cor_15

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2015<-graf_signed(graf_cor_15,cor_15,media_cor_15)

#Converte a matriz de correlação em vetor
vet_cor_15<-matrix_vet(cor_15,vet_cor_15)


#cria gráficos com e sem outlieres
box_15<-boxplot(vet_cor_15)
min(box_15$out)
vet_cor_15_so<-vet_cor_15[vet_cor_15>=box_15$stats[1] & vet_cor_15<=box_15$stats[5]]
media_cor_15_so <- mean(vet_cor_15_so)

par(mfrow=c(2,2))
hist(vet_cor_15)
plot(density(vet_cor_15))

hist(vet_cor_15_so)
plot(density(vet_cor_15_so))



#write.csv2(fecha_15,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2015.csv")

#write.csv2(fecha_15_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_15_op.csv")
#write.csv2(fecha_15_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_15_op_ajus.csv")
#write.csv2(fecha_15_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_15_return.csv")

#write.csv2(cor_15,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_15.csv")
#write.csv2(signed_2015,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2015.csv")
#write.csv2(vet_cor_15_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_15_so.csv")
#write.csv2(vet_cor_15,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_15.csv")




######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

fecha_16<-filter(fechamento,year(date)==2016)

#filtra moedas que tem no mínimo 6 meses)
fecha_16_op<-fecha_16[,c("X1337coin","X42.coin","X8bit","advanced.internet.blocks","adzcoin","aeon","agoras.tokens","amsterdamcoin","anarchistsprime","anoncoin","artbyte","audiocoin","auroracoin","bata","bean.cash","belacoin","bitbar","bitbay","bitbtc","bitcny","bitcoin","bitcoin.plus","bitcrystals","bitmark","bitquark","bitsend","bitshares","bitstar","bitswift","bitusd","bitzeny","blackcoin","blakecoin","blocknet","bolivarcoin","boolberry","boostcoin","bunnycoin","burst","bytecoin.bcn","canada.ecoin","cannabiscoin","capricoin","casinocoin","circuits.of.value","clams","cloakcoin","clubcoin","coin2.1","counterparty","creditbit","crevacoin","crown","cryptonite","curecoin","dash","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","dimecoin","dnotes","dogecoin","dopecoin","e.gulden","eccoin","einsteinium","emerald","emercoin","energycoin","ethereum","eurocoin","evergreencoin","exclusivecoin","expanse","factom","faircoin","feathercoin","fedoracoin","flo","foldingcoin","fujicoin","gambit","gamecredits","gcn.coin","geocoin","global.currency.reserve","globalboost.y","goldcoin","gridcoin","groestlcoin","guccionecoin","gulden","hempcoin","huntercoin","hyperstake","incakoin","infinitecoin","iocoin","joincoin","leocoin","litecoin","litedoge","magi","maidsafecoin","manna","maxcoin","megacoin","mintcoin","moin","monacoin","monero","monetaryunit","mooncoin","myriad","namecoin","nav.coin","nem","neoscoin","neutron","newyorkcoin","nexus","novacoin","nubits","nushares","nxt","nyancoin","obits","okcash","omni","orbitcoin","paccoin","pakcoin","pandacoin.pnd","parallelcoin","paycoin2","peercoin","pesetacoin","petrodollar","piggycoin","pinkcoin","popularcoin","potcoin","prime.xi","primecoin","pura","quark","quotient","radium","ratecoin","reddcoin","rimbit","ripple","rubycoin","secretcoin","securecoin","sexcoin","shift","siacoin","sibcoin","smileycoin","solarcoin","songcoin","sphere","spreadcoin","startcoin","stealth","stellar","storjcoin.x","swing","synereo","synergy","syscoin","tagcoin","tekcoin","terracoin","teslacoin","tether","titcoin","transfercoin","trollcoin","ubiq","ultracoin","universal.currency","unobtanium","verge","vericoin","vertcoin","viacoin","virtacoin","whitecoin","wild.beast.block","worldcoin","x.coin","xaurum","zeitcoin","zetacoin")]

#Substui dia sem negociação pelo dia anterior por dia anterior
fecha_16_op_ajus<-fecha_16_op


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_16_op_ajus<-ajust_dia(fecha_16_op,fecha_16_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_16_op_ajus->fecha_16_return
fecha_16_return<-log_retrono(fecha_16_op_ajus,fecha_16_return)


#Função que cria correlação média

fecha_16_return[sapply(fecha_16_return, is.infinite)] <- 0
fecha_16_return[is.na(fecha_16_return)] <- 0


cor_16<-cor(fecha_16_return)



media_cor_16<-media_matri(cor_16,media_cor_16)


graf_cor_16<-cor_16
#Converte a matriz de correlação em vetor



#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2016<-graf_signed(graf_cor_16,cor_16,media_cor_16)


vet_cor_16<-matrix_vet(cor_16,vet_cor_16)

#cria gráficos com e sem outlieres
box_16<-boxplot(vet_cor_16)
vet_cor_16_so<-vet_cor_16[vet_cor_16>=box_16$stats[1] & vet_cor_16<=box_16$stats[5]]
media_cor_16_so <- mean(vet_cor_16_so)
par(mfrow=c(2,2))


hist(vet_cor_16)
plot(density(vet_cor_16))

hist(vet_cor_16_so)
plot(density(vet_cor_16_so))


#write.csv2(fecha_16,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2016.csv")

#write.csv2(fecha_16_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_16_op.csv")
#write.csv2(fecha_16_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_16_op_ajus.csv")
#write.csv2(fecha_16_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_16_return.csv")

#write.csv2(cor_16,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_16.csv")
#write.csv2(signed_2016,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2016.csv")
#write.csv2(vet_cor_16_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_16_so.csv")
#write.csv2(vet_cor_16,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_16.csv")



######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################



fecha_17<-filter(fechamento,year(date)==2017)


#filtra moedas que tem no mínimo 6 meses)
fecha_17_op<-fecha_17[,c("X1337coin","X2give","X42.coin","X808coin","X8bit","aces","acoin","advanced.internet.blocks","adzcoin","aeon","agoras.tokens","allsafe","amsterdamcoin","anarchistsprime","anoncoin","aquariuscoin","arcticcoin","ardor","argentum","artbyte","asiadigicoin","atomic.coin","audiocoin","augur","auroracoin","aurumcoin","avatarcoin","b3coin","bata","bean.cash","belacoin","benjirolls","berncash","bitbar","bitbay","bitbtc","bitcloud","bitcny","bitcoin","bitcoin.21","bitcoin.plus","bitcoin.scrypt","bitcrystals","biteur","bitgold","bitmark","bitquark","bitsend","bitshares","bitsilver","bitstar","bitswift","bitusd","bitzeny","blackcoin","blakecoin","blocknet","bluecoin","bolivarcoin","boostcoin","bowscoin","breakout","breakout.stake","btctalkcoin","bumbacoin","bunnycoin","burst","bytecoin.bcn","c.bit","californium","canada.ecoin","cannabiscoin","capricoin","cashcoin","chesscoin","circuits.of.value","clams","cloakcoin","clubcoin","coin2.1","comet","counterparty","creditbit","crevacoin","crown","cryptocarbon","cryptojacks","cryptonite","curecoin","darcrus","dash","decent","decred","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","digixdao","dimecoin","dnotes","dogecoin","dopecoin","e.dinar.coin","e.gulden","eccoin","edrcoin","einsteinium","elcoin.el","elementrem","emerald","emercoin","energycoin","espers","eternity","ethereum","ethereum.classic","eurocoin","evergreencoin","evil.coin","exclusivecoin","expanse","factom","faircoin","fargocoin","feathercoin","fedoracoin","first.bitcoin","firstblood","flo","foldingcoin","francs","fujicoin","fuzzballs","gambit","gamecredits","gcn.coin","geocoin","global.currency.reserve","globalboost.y","goldblocks","goldcoin","golem.network.tokens","golos","gridcoin","groestlcoin","guccionecoin","gulden","heat.ledger","hempcoin","hicoin","hodlcoin","huntercoin","hush","hyperstake","ico.openledger","iconomi","incakoin","incent","independent.money.system","infinitecoin","inflationcoin","internet.of.people","iocoin","ion","ixcoin","joincoin","joulecoin","karbo","kobocoin","korecoin","kurrent","lanacoin","leocoin","library.credit","lisk","litecoin","litecred","litedoge","lomocoin","luna.coin","lykke","magi","maidsafecoin","manna","martexcoin","maxcoin","megacoin","memetic","mintcoin","moin","mojocoin","monacoin","monero","monetaryunit","mooncoin","motocoin","mustangcoin","myriad","namecoin","nav.coin","nem","neo","neoscoin","neutron","nevacoin","newyorkcoin","nexium","nexus","nolimitcoin","novacoin","nubits","nullex","nushares","nxt","nyancoin","obits","obyte","okcash","omni","orbitcoin","pabyosi.coin.special","paccoin","pakcoin","pandacoin.pnd","parallelcoin","parkbyte","pascal.coin","paycoin2","peercoin","pepe.cash","pesetacoin","petrodollar","photon","piggycoin","pinkcoin","pivx","plncoin","pluton","popularcoin","posex","postcoin","posw.coin","potcoin","president.johnson","president.trump","prime.xi","primecoin","pura","purevidz","putincoin","quark","quebecoin","qwark","radium","ratecoin","reddcoin","revolutionvr","rimbit","ripple","rise","rubies","rubycoin","safe.exchange.coin","salus","securecoin","sequence","sexcoin","shift","siacoin","sibcoin","singulardtv","sixeleven","smartcoin","smileycoin","solarcoin","songcoin","spectrecoin","sphere","spreadcoin","startcoin","stealth","steem","steem.dollars","stellar","storjcoin.x","stratis","swing","syndicate","synereo","synergy","syscoin","tagcoin","tajcoin","tekcoin","terracoin","teslacoin","tether","titcoin","tittiecoin","transfercoin","trollcoin","trumpcoin","ubiq","universal.currency","unobtanium","vaperscoin","veltor","verge","vericoin","veriumreserve","veros","vertcoin","viacoin","vslice","waves","whitecoin","wild.beast.block","wings","worldcoin","x.coin","xaurum","yocoin","zayedcoin","zcash","zclassic","zcoin","zeitcoin","zetacoin","zurcoin")]

#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_17_op_ajus<-fecha_17_op
fecha_17_op_ajus<-ajust_dia(fecha_17_op,fecha_17_op_ajus)


#função gera o log do retorno das negociaçoes

fecha_17_return<-fecha_17_op_ajus
fecha_17_return<-log_retrono(fecha_17_op_ajus,fecha_17_return)


#substitui NA e inf por 0

fecha_17_return[sapply(fecha_17_return, is.infinite)] <- 0
fecha_17_return[is.na(fecha_17_return)] <- 0


cor_17<-cor(fecha_17_return)
#Função que cria correlação média
media_cor_17<-media_matri(cor_17,media_cor_17)



graf_cor_17<-cor_17

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2017<-graf_signed(graf_cor_17,cor_17,media_cor_17)


#Converte a matriz de correlação em vetor
vet_cor_17<-matrix_vet(cor_17,vet_cor_17)

#cria gráficos com e sem outlieres

box_17<-boxplot(vet_cor_17)
min(box_17$out)
vet_cor_17_so<-vet_cor_17[vet_cor_17>=box_17$stats[1] & vet_cor_17<=box_17$stats[5]]
media_cor_17_so <- mean(vet_cor_17_so)

par(mfrow=c(2,2))


hist(vet_cor_17)
plot(density(vet_cor_17))


hist(vet_cor_17_so)
plot(density(vet_cor_17_so))


#write.csv2(fecha_17,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2017.csv")

#write.csv2(fecha_17_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_17_op.csv")
#write.csv2(fecha_17_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_17_op_ajus.csv")
#write.csv2(fecha_17_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_17_return.csv")

#write.csv2(cor_17,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_17.csv")
#write.csv2(signed_2017,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2017.csv")
#write.csv2(vet_cor_17_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_17_so.csv")
#write.csv2(vet_cor_17,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_17.csv")



######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

fecha_18<-filter(fechamento,year(date)==2018)


#filtra moedas que tem no mínimo 6 meses)
m1<-c("X0x","X1337coin","X2give","X42.coin","X808coin","X8bit","ace","achain","acoin","adcoin","adshares","adtoken","advanced.internet.blocks","adx.net","adzcoin","aelf","aeon","aeron","aeternity","agrello.delta","aichain","aidoc","aidos.kuneen","aigang","aion","airswap","alis","allion","allsafe","alqo","amber","amsterdamcoin","anarchistsprime","anoncoin","appcoins","aquariuscoin","aragon","arbit","arbitragect","arcticcoin","ardor","argentum","argus","ark","artbyte","asch","asiadigicoin","atbcoin","atlant","atmos","atn","atomic.coin","attention.token.of.media","audiocoin","augur","auroracoin","aurumcoin","authorship","autonio","aventus","axiom","b2bx","b3coin","bancor","bankcoin","bankex","basic.attention.token","bastonet","bata","bean.cash","belacoin","benjirolls","berncash","betacoin","biblepay","bibox.token","bigup","billionaire.token","binance.coin","bitbar","bitbase","bitbay","bitbtc","bitclave","bitcloud","bitcny","bitcoal","bitcoin","bitcoin.atom","bitcoin.cash","bitcoin.diamond","bitcoin.god","bitcoin.gold","bitcoin.planet","bitcoin.plus","bitcoin.red","bitcoin.scrypt","bitcoinx","bitcoinz","bitcore","bitcrystals","bitdeal","bitdegree","bitdice","biteur","bitgold","bitmark","bitquark","bitqy","bitradio","bitsend","bitshares","bitsilver","bitstar","bitusd","bitvolt","bitzeny","blackcoin","blackmoon","blakecoin","blakestar","blazercoin","blockcat","blockcdn","blockmason","blocknet","blocktix","blockv","bloomtoken","blox","bluecoin","bodhi","bolivarcoin","bonpay","boolberry","boscoin","bottos","bounty0x","bowscoin","brat","bread","breakout","breakout.stake","briacoin","bridgecoin","btcmoon","bulwark","bumbacoin","bunnycoin","burst","buzzcoin","bytecoin.bcn","bytom","c.bit","cabbage","californium","campuscoin","canada.ecoin","cannabiscoin","canyacoin","cappasity","capricoin","carboncoin","cardano","cashcoin","ccore","cdx.network","centurion","cfun","chainlink","change","chesscoin","chips","chronobank","chronologic","cindicator","circuits.of.value","civic","clams","clearpoll","cloakcoin","clubcoin","cobinhood","cofound.it","coimatic.2","coimatic.3","coin2.1","coinlancer","coinonat","coinonatx","colossusxt","comet","comsa.eth","comsa.xem","condensate","content.and.ad.network","coss","counterparty","coupecoin","crave","crea","cream","creditbit","credo","crevacoin","crown","crypto.com","cryptocarbon","cryptojacks","cryptonex","cryptonite","cryptopay","cryptoping","crystal.clear","curecoin","cvcoin","cybermiles","dai","dalecoin","dao.casino","darcrus","dash","databits","datum","decent","decent.bet","decentraland","decision.token","decred","deepbrain.chain","deeponion","delphy","denarius.dnr","dent","dentacoin","desire","deutsche.emark","dew","diamond","digibyte","digital.money.bits","digitalcoin","digitalnote","digitalprice","digixdao","dimcoin","dimecoin","dinastycoin","district0x","dix.asset","dogecoin","domraider","dopecoin","doubloon","dovu","draftcoin","dragonchain","dubaicoin.dbix","dutch.coin","dynamic","dynamic.trading.rights","dynamiccoin","e.dinar.coin","e.gulden","ea.coin","eboostcoin","ebtcnew","eccoin","echolink","ecobit","ecocoin","edgeless","edrcoin","eidoo","einsteinium","elcoin.el","electra","electroneum","elementrem","elixir","ellaism","eltcoin","elysium","emerald","emercoin","emphy","encrypgen","encryptotel","encryptotel.eth","energo","enigma","enjin.coin","eos","equitrader","erc20","ergo","eroscoin","eryllium","espers","eternity","ethbet","ethbits","ethereum","ethereum.blue","ethereum.classic","ethereum.gold","ethereum.lite","ethereumcash","etheroll","etherparty","ethlend","ethos","eventchain","everex","evergreencoin","everus","evil.coin","exchange.union","exclusivecoin","expanse","experience.points","exrnchain","factom","faircoin","fairgame","fargocoin","feathercoin","fedoracoin","fidentiax","filecoin","first.bitcoin","firstblood","firstcoin","flash","flik","flixxo","flo","flypme","foldingcoin","force","francs","freicoin","fujicoin","fujinto","funfair","fuzzballs","galactrum","gambit","game","gamechain","gamecredits","gas","gcn.coin","geertcoin","genaro.network","genesis.vision")
m2<-c("geysercoin","gifto","global.cryptocurrency","global.currency.reserve","globalboost.y","globaltoken","gnosis.gno","gobyte","gold.reward.token","goldblocks","goldcoin","golem.network.tokens","golfcoin","golos","golos.gold","goodomy","gravitycoin","grid","gridcoin","grimcoin","groestlcoin","guccionecoin","gulden","guppy","gxchain","hacken","happycoin","harmonycoin.hmc","heat.ledger","helleniccoin","hellogold","hempcoin","herocoin","hicoin","high.performance.blockchain","high.voltage","hiveterminal.token","hodlcoin","hollywoodcoin","homeblockcoin","honey","html.coin","humaniq","huntercoin","hush","hyper.pay","hypercash","hyperstake","icon","iconic","iconomi","iethereum","ignis","ignition","incakoin","incent","indorse.token","infinity.economics","inflationcoin","ink","innova","insanecoin.insn","insolar","insurepal","intelligent.trading.foundation","internet.node.token","internet.of.people","internet.of.things","internxt","investfeed","iocoin","ion","iostoken","iot.chain","iota","iquant","irishcoin","iticoin","ixcoin","ixledger","jetcoin","joincoin","joulecoin","karbo","karma","kcash","kekcoin","kickico","kin","kobocoin","kolion","komodo","korecoin","kucoin.shares","kurrent","kyber.network","kz.cash","lamden","lampix","lanacoin","latoken","leocoin","lethean","leverj","library.credit","life","lightning.bitcoin","linda","linx","lisk","litebitcoin","litecoin","litecoin.plus","litecoin.ultra","litedoge","lockchain","lomocoin","loopring","luna.coin","lunyr","luxcoin","maecenas","magi","maidsafecoin","maker","manna","mao.zedong","martexcoin","master.swiscoin","matryx","maverick.chain","maxcoin","mcap","measurable.data.token","medibloc","medishares","melon","memetic","mercury","metal","metaverse","micromoney","mincoin","minereum","miners.reward.token","minex","minexcoin","mintcoin","mobilego","modum","moeda.loyalty.points","moin","mojocoin","monacocoin","monacoin","monero","monetaryunit","monetha","monkey.project","mooncoin","more.coin","motocoin","msd","musicoin","mustangcoin","mybit","myriad","mysterium","mywish","naga","namecoin","nano","nav.coin","neblio","nebulas.token","nekonium","nem","neo","neoscoin","netko","neumark","neuro","neutron","nevacoin","neverdie","newyorkcoin","nexium","nexus","nitro","nolimitcoin","novacoin","nubits","nullex","nuls","numeraire","nushares","nxt","nyancoin","oax","obits","obsidian","obyte","oceanlab","okcash","olympus.labs","omisego","omni","oneroot.network","ongsocial","onix","op.coin","opal","open.trading.network","opus","oraclechain","orbitcoin","ormeus.coin","ost","pabyosi.coin.special","paccoin","pakcoin","pandacoin.pnd","paragon","parallelcoin","particl","pascal.coin","patientory","paycoin2","payfair","paypie","peepcoin","peercoin","peerplays.ppy","pepe.cash","pesetacoin","petrodollar","phantomx","phore","piggycoin","pillar","pinkcoin","piplcoin","pirl","pivx","platinumbar","playercoin","playkey","pluton","poet","polis","poly.ai","polybius","popularcoin","populous","postcoin","posw.coin","potcoin","power.ledger","presearch","president.johnson","president.trump","primalbase","primas","prime.xi","primecoin","privatix","prizm","prochain","procurrency","profile.utility.token","project.x","propy","proud.money","pura","purevidz","putincoin","pylon.network","qash","qbao","qlink","qtum","quantstamp","quantum.resistant.ledger","quark","qube","quebecoin","qunqun","quotient","qwark","rabbitcoin","radium","raiden.network.token","ratecoin","rchain","real","realchain","rebl","red.pulse","reddcoin","regalcoin","renos","request","revain","revolutionvr","rimbit","ripio.credit.network","ripple","rise","rivetz","rlc","roulettetoken","rubies","rubycoin","runners","rupaya","rupee","safe.exchange.coin","safe.trade.coin","salt","salus","santiment","save.and.gain","securecoin","segwit2x","selfkey","sequence","shadow.token","sharechain","shield.xsh","shift","show","siacoin","sibcoin","singulardtv","singularitynet","sirin.labs.token","sixeleven","skeincoin","skincoin","skycoin","smartcash","smartcoin","smartmesh","smileycoin","snovio","soarcoin","social.send","socialcoin.socc","sociall")
m3<-c("solarcoin","solaris","soma","songcoin","sonm","sophiatx","spacechain","spankchain","spectre.utility","spectrecoin","speedcash","sphere","sphere.identity","sportyco","spreadcoin","sprouts","starta","startcoin","status","stealth","steem","steem.dollars","stellar","steneum.coin","storj","storjcoin.x","storm","stox","straks","stratis","streamr.datacoin","stronghands","student.coin","substratum","sugar.exchange","sumokoin","suncontract","super.bitcoin","supercoin","suretly","swarm.city","swftcoin","swing","swisscoin","syndicate","synereo","syscoin","taas","tael","tagcoin","tajcoin","target.coin","tekcoin","telcoin","tellurion","tenx","terracoin","terranova","teslacoin","tether","tezos","the.champcoin","theresa.may.coin","theta","tierion","tiesdb","time.new.bank","titcoin","tittiecoin","toacoin","tokencard","tokenclub","tokes","tokyo","topchain","tracto","transfercoin","trezarcoin","trident","trollcoin","tron","trueflip","trumpcoin","trust","ubiq","ugchain","ultimate.secure.cash","ultracoin","uniform.fiscal.object","unify","unikoin.gold","united.bitcoin","universal.currency","universe","unobtanium","upfiring","uquid.coin","utrust","uttoken","veltor","verge","vericoin","verify","veritaseum","veriumreserve","veros","version","vertcoin","vezt","viacoin","vibe","viberate","virtacoin","viuly","vivo","voisecom","votecoin","vslice","vsync.vsx","wagerr","waltonchain","wandx","waves","waves.community.token","wavesgo","wax","waykichain","wearesatoshi","weth","whalecoin","whitecoin","wi.coin","wild.beast.block","win.coin","wings","women","worldcoin","worldcore","x.coin","xaurum","xel","xenon","xgox","xpa","xtrabytes","yenten","yocoin","yoyow","zap","zcash","zclassic","zcoin","zeitcoin","zencash","zengold","zero","zetacoin","zeusshield","zozocoin","zrcoin")

fecha_18_op<-fecha_18[c(m1,m2,m3)]

#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_18_op_ajus<-fecha_18_op
fecha_18_op_ajus<-ajust_dia(fecha_18_op,fecha_18_op_ajus)


#função gera o log do retorno das negociaçoes

fecha_18_return<-fecha_18_op_ajus
fecha_18_return<-log_retrono(fecha_18_op_ajus,fecha_18_return)


#substitui NA e inf por 0

fecha_18_return[sapply(fecha_18_return, is.infinite)] <- 0
fecha_18_return[is.na(fecha_18_return)] <- 0


cor_18<-cor(fecha_18_return)

#Função que cria correlação média
media_cor_18<-media_matri(cor_18,media_cor_18)




graf_cor_18<-cor_18

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2018<-graf_signed(graf_cor_18,cor_18,media_cor_18)

#Converte a matriz de correlação em vetor
vet_cor_18<-matrix_vet(cor_18,vet_cor_18)


#cria gráficos com e sem outlieres

box_18<-boxplot(vet_cor_18)
vet_cor_18_so<-vet_cor_18[vet_cor_18>=box_18$stats[1] & vet_cor_18<=box_18$stats[5]]
media_cor_18_so <- mean(vet_cor_18_so)
par(mfrow=c(2,2))


hist(vet_cor_18)
plot(density(vet_cor_18))

hist(vet_cor_18_so)
plot(density(vet_cor_18_so))




#write.csv2(fecha_18,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2018.csv")

#write.csv2(fecha_18_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_18_op.csv")
#write.csv2(fecha_18_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_18_op_ajus.csv")
#write.csv2(fecha_18_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_18_return.csv")

#write.csv2(cor_18,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_18.csv")
#write.csv2(signed_2018,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_2018.csv")
#write.csv2(vet_cor_18_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_18_so.csv")
#write.csv2(vet_cor_18,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_18.csv")

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

fecha_19<-filter(fechamento,year(date)==2019)



#filtra moedas que tem no mínimo 6 meses)
m1<-c("X0chain","X0x","X0xbtc","X0xcert","X1337coin","X1world","X2give","X42.coin","X4new","X808coin","X8bit","ab.chain.rtb","abbc.coin","abcc.token","absolute","abulaba","ac3","acchain","ace","aced","aces","achain","acoin","acre","actinium","acute.angle.cloud","adbank","adcoin","adelphoi","adenz","adhive","aditus","adshares","adtoken","adultchain","advanced.internet.blocks","adx.net","adzcoin","aegeus","aelf","aeon","aergo","aeron","aeternity","agrello.delta","agrolot","aichain","aidcoin","aidoc","aidos.kuneen","aigang","ailink.token","aion","airbloc","airswap","airwire","akroma","alax","alchemint.standards","alis","all.sports","allcoin","allion","allsafe","alpha.coin","alphacat","alqo","alt.estate.token","altcommunity.coin","alttex","amber","amlt","ammo.reloaded","amo.coin","amon","amsterdamcoin","anarchistsprime","animation.vision.cash","anon","anoncoin","apex","aphelion","apis","apollo.currency","apollon","apot","appcoins","apr.coin","aquariuscoin","aragon","arbidex","arbit","arbitragect","arcblock","archetypal.network","arcticcoin","ardor","arepacoin","argentum","argus","arion","arionum","ark","artbyte","asch","asiadigicoin","aston","asura.coin","atbcoin","atc.coin","atheios","atlant","atlantis.blue.digital.token","atlas.protocol","atmos","atn","atomic.coin","atonomi","attention.token.of.media","auctus","audiocoin","augur","aurora","aurora.dao","auroracoin","aurumcoin","authorship","autonio","auxilium","avatarcoin","aventus","avinoc","aware","axiom","axpire","azart","b2bcoin","b2bx","b3coin","baasid","babb","baer.chain","banca","bancor","bank.coin","bankcoin","bankera","bankex","banyan.network","basic.attention.token","bastonet","bata","bbscoin","beacon","bean.cash","beat","bee.token","beekan","beetle.coin","belacoin","benjirolls","benz","berncash","betacoin","bethereum","betterbetting","bettex.coin","bezant","bezop","bffdoom","bgogo.token","bhpcash","biblepay","bibox.token","bigbom","bigup","billionaire.token","binance.coin","bingocoin","biocoin","bionic","biotron","birake","birds","bit.tube","bit.z.token","bitbar","bitbase","bitbay","bitblocks","bitbtc","bitcapitalvendor","bitclave","bitcloud","bitcny","bitcoal","bitcoen","bitcoin","bitcoin.21","bitcoin.adult","bitcoin.atom","bitcoin.cash","bitcoin.diamond","bitcoin.file","bitcoin.god","bitcoin.gold","bitcoin.green","bitcoin.incognito","bitcoin.interest","bitcoin.one","bitcoin.planet","bitcoin.plus","bitcoin.private","bitcoin.red","bitcoin.scrypt","bitcoin.sv","bitcoin.token","bitcoin.w.spectrum","bitcoin.x","bitcoin.zero","bitcoin2network","bitcoinote","bitcoinus","bitcoinx","bitcoinz","bitcore","bitcrystals","bitdeal","bitdegree","bitdepositary","bitdice","bitether","biteur","bitgold","bitguild.plat","bitibu.coin","bitkan","bitmark","bitmart.token","bitmoney","bitnation","bitnautic.token","bitnewchain","bitquark","bitqy","bitradio","bitrent","bitrewards","bitscreener.token","bitsend","bitshares","bitshares.music","bitsilver","bitspace","bitstar","bitstation","bitsum","bitswift","bittwatt","bitup.token","bitusd","bitvolt","bitwhite","bitzeny","blackcoin","blackmoon","blakecoin","blakestar","blast","blazercoin","blitzpredict","bloc.money","block.array","block.chain.com","block.logic","blockcat","blockcdn","blockchain.certified.data.token","blockchain.quotations.index.token","blocklancer","blockmason","blockmesh","blocknet","blocknode","blockparty.boxx.token","blockpass","blockport","blocktix","blocktrade.token","blockv","bloomtoken","blox","blue.whale.token","bluecoin","bluzelle","bnktothefuture","bobs.repair","bodhi","bodhi.eth")
m2<-c("bolenum","bolivarcoin","bonpay","boolberry","boostcoin","boscoin","bottos","bounty0x","boutspro","bowscoin","box.token","brahmaos","brat","bread","breakout","breakout.stake","breezecoin","briacoin","brickblock","bridge.protocol","bridgecoin","britcoin","brokernekonetwork","btcmoon","btctalkcoin","bubble","budbo","buggyra.coin.zero","bulwark","bumbacoin","bumo","bunnycoin","bunnytoken","burst","business.credit.alliance.chain","buzzcoin","bytecoin.bcn","bytom","bzlcoin","c.bit","c20","cabbage","californium","callisto.network","campuscoin","canada.ecoin","candy","cannabiscoin","cannation","canyacoin","capdaxtoken","cappasity","capricoin","carat","carblock","carboncoin","carboneum.c8.token","cardano","cardbuyers","cardstack","carebit","cargox","carinet","carlive.chain","carvertical","cashaa","cashbery.coin","cashbet.coin","cashcoin","casinocoin","castle","catocoin","cazcoin","ccore","cdx.network","cedex.coin","ceek.vr","celsius","centaure","centrality","centurion","cfun","chainlink","change","chatcoin","cheesecoin","chesscoin","chex","chips","chronobank","chronologic","cindicator","circuits.of.value","citadel","civic","civitas","cjs","ckusd","clams","clearcoin","clearpoll","clipper.coin","cloakcoin","clubcoin","cmitcoin","cobinhood","cobrabytes","cofound.it","coimatic.2","coimatic.3","coin.lion","coin2.1","coin2play","coinex.token","coinfi","coinlancer","coinmeet","coinonat","coinonatx","coinpoker","coinsuper.ecosystem.network","cointogo","coinus","coinvest","colossusxt","colu.local.network","comet","commerce.data.connection","commerceblock","commercium","communitygeneration","compound.coin","comsa.eth","comsa.xem","concierge.coin","concoin","condensate","condominium","coni","connectjob","cononchain","consensus","consentium","constellation","content.and.ad.network","content.neutrality.network","contentbox","contractnet","copytrack","cortex","cosmo.coin","coss","cottoncoin","couchain","counterparty","coupecoin","cova","covesting","cpchain","cpollo","crave","crea","cream","credit.tag.chain","creditbit","credits","credo","crevacoin","croat","cropcoin","crowd.machine","crowdholding","crowdwiz","crown","cruisebit","crycash","cryptaldash","cryptaur","crypterium","crypticcoin","crypto.com","crypto.com.chain","crypto.harbor.exchange","crypto.improvement.fund","cryptocarbon","cryptoflow","cryptojacks","cryptonex","cryptonite","cryptopay","cryptoping","cryptosolartech","cryptosoul","cryptrust","crystal.clear","crystal.token","cube","curecoin","cvcoin","cwv.chain","cyber.movie.chain","cybereits","cyberfm","cybermiles","cybermusic","cybervein","cyclean","dacc","dacsee","dadi","daex","dai","dalecoin","daneel","dao.casino","daostack","daps.token","darcio.ecosystem.coin","darcrus","darextravel","darkpaycoin","dascoin","dash","data","data.exchange","databits","datacoin","datarius.credit","datawallet","datum","datx","dav.coin","davinci.coin","debitum.network","decent","decent.bet","decentraland","decentralized.asset.trading.platform","decentralized.machine.learning","decision.token","decred","deepbrain.chain","deeponion","deex","delizia","delphy","delta.chain","denarius.dnr","dent","dentacoin","dero","desire","dether","deutsche.emark","devery","deviantcoin","dew","dex","diamond","dietbitcoin","digibyte","digifinextoken","digital.asset.exchange.token","digital.asset.guarantee.token","digital.insurance.token","digital.money.bits","digitalcoin","digitalnote","digitalprice","digitex.futures","digiwage","digix.gold.token","digixdao","dignity","dimcoin","dimecoin","dinastycoin","dinero","dipnet","distributed.credit.chain","district0x","divi","dix.asset","dmarket","dnotes","doc.com.token","dock","dogecoin","dollarcoin","domraider","dopecoin","dorado","doubloon","dovu","dowcoin","dprating","draftcoin","dragon.coins","dragon.token")
m3<-c("dragonchain","dragonglass","dreamcoin","dropil","drp.utility","dubaicoin.dbix","dutch.coin","dws","dxchain.token","dynamic","dynamic.trading.rights","dynamiccoin","dystem","e.chat","e.dinar.coin","e.gulden","ea.coin","eaglex","earth.token","ebcoin","eboostcoin","ebtcnew","eccoin","echolink","ecobit","ecocoin","ecoreal.estate","eden","edgeless","edrcoin","edu.coin","educare","education.ecosystem","effect.ai","egretia","eidoo","einsteinium","elastos","elcoin.el","electra","electrifyasia","electroneum","elementrem","eligma.token","elixir","ellaism","elliot.coin","eltcoin","elysian","elysium","emaratcoin","embercoin","emerald","emercoin","emphy","empowr.coin","encrypgen","encryptotel","encryptotel.eth","endor.protocol","endorsit","energi","energitoken","energo","engagement.token","engine","enigma","enjin.coin","envion","eos","eosblack","eosdac","eplus.coin","equal","equitrader","erc20","ergo","eristica","eroscoin","eryllium","escroco.emerald","escrowcoin","esdchain","espers","esports.token","essentia","eternal.token","eternity","ethbet","ethbits","etheera","ether.1","ether.kingdoms.token","ether.zero","ethereum","ethereum.blue","ethereum.classic","ethereum.gold","ethereum.lite","ethereum.meta","ethereumcash","ethergem","etherinc","etheriya","etheroll","etherparty","ethersocial","ethersportz","ethlend","ethorse","ethos","euno","eunomia","eurocoin","evencoin","eventchain","everex","evergreencoin","everipedia","everus","evil.coin","evimeria","excaliburcoin","exchange.union","exclusivecoin","exmr","expanse","experience.points","experience.token","experty","exrnchain","ezoow","eztoken","fabric.token","faceter","factom","faircoin","fairgame","fanstime","fantasygold","fantom","fargocoin","farmatrust","fcoin.token","feathercoin","fedoracoin","fidelium","fidentiax","fiii","filecoin","fintab","fintrux.network","fire.lotto","first.bitcoin","firstblood","firstcoin","fivebalance","flash","flik","flip","flixxo","flo","fluz.fluz","flypme","fnkos","foam","foin","foldingcoin","folmcoin","food","footballcoin","force","forkcoin","formosa.financial","fortuna","fountain","fox.trading","francs","free.coin","freicoin","freyrchain","friends","fsbt.api.token","fujicoin","fujinto","fundrequest","fundtoken","funfair","fusion","futurax","future1coin","futurocoin","fuzex","fuzzballs","galactrum","gambit","gamblecoin","game","game.stars","gamechain","gamecredits","garlicoin","gas","gazecoin","gcn.coin","geertcoin","gemini.dollar","gems.protocol","genaro.network","gene.source.code.chain","genesis.vision","genesisx","gentarium","geocoin","get.protocol","geysercoin","giant.coin","gifto","giga","gincoin","gladius.token","global.awards.token","global.cryptocurrency","global.currency.reserve","global.social.chain","globalboost.y","globaltoken","globalvillage.ecosystem","gnosis.gno","gobyte","gochain","gohelpfund","gold.bits.coin","gold.poker","gold.reward.token","goldblocks","goldcoin","goldmint","golem.network.tokens","golfcoin","golos","golos.gold","gonetwork","goodomy","gossipcoin","graft","graphcoin","graviocoin","gravity","gravitycoin","greenmed","grid","gridcoin","grimcoin","groestlcoin","gsenetwork","guaranteed.ethurance.token.extra","guccionecoin","guess","gulden","guppy","gxchain","hacken","halalchain","halloween.coin","happycoin","haracoin","harmonycoin.hmc","hashcoin","hashgard","haven.protocol","havy","hbz.coin","hdac","heartbout","heat.ledger","hedgetrade","helium","helleniccoin","hellogold","help.the.homeless.coin","helper.search.token","hempcoin","herbalist.token","hercules","herocoin","heronode","hi.mutual.society","hicoin","high.performance.blockchain","high.voltage","hitchain","hiveterminal.token","hodlcoin","hold","hollywoodcoin","holo","homeblockcoin","hondaiscoin")
m4<-c("honey","hoqu","horuspay","howdoo","html.coin","hubii.network","humaniq","humanscape","huntercoin","huobi.token","hurify","hush","huzu","hybrid.block","hycon","hydro.protocol","hydrogen","hyper.pay","hypercash","hyperquant","hyperstake","ibank","ibtc","ice.rock.mining","ico.openledger","icobid","icon","iconic","iconiq.lab.token","iconomi","idealcash","idex.membership","idol.coin","iethereum","ifoods.chain","ignis","ignition","igtoken","iht.real.estate.protocol","ilcoin","imbrex","impact","incakoin","incent","incodium","indahash","independent.money.system","indinode","indorse.token","infinipay","infinitecoin","infinity.economics","inflationcoin","influence.chain","ink","ink.protocol","innova","ino.coin","insanecoin.insn","insight.chain","insights.network","insolar","insurchain","insurepal","insureum","intelligent.investment.chain","intelligent.trading.foundation","intercrone","internationalcryptox","internet.node.token","internet.of.people","internet.of.things","internxt","intervalue","interzone","invacio","investdigital","investfeed","invictus.hyperion.fund","iocoin","ion","ionchain","iostoken","iot.chain","iota","iotex","ip.exchange","ipchain","iqcash","iqeon","iquant","iridium","irishcoin","italian.lira","iticoin","iungo","ivy","ixcoin","ixledger","javascript.token","jesus.coin","jet8","jetcoin","jibrel.network","jin.coin","jingtum.tech","jiyo","jiyo.old","joincoin","joint.ventures","joulecoin","jsecoin","jury.online.token","kalkulus","kambria","kanadecoin","karatgold.coin","karbo","karma","karma.eos","kcash","kekcoin","key","kickico","kin","kind.ads.token","kingn.coin","kingxchain","kleros","knekted","know","knoxstertoken","kobocoin","kolion","komodo","kora.network.token","korecoin","kryll","kucoin.shares","kun","kurrent","kwhcoin","kyber.network","kz.cash","labh.coin","lala.world","lambda","lamden","lampix","lanacoin","latiumx","latoken","leadcoin","legolas.exchange","lemochain","lendingblock","lendroid.support.token","leocoin","lethean","level.up","leverj","libra.credit","library.credit","life","lightchain","lightning.bitcoin","lightpaycoin","likecoin","lina","linda","linkey","linkeye","linx","liquidity.network","lisk","lisk.machine.learning","litebitcoin","litecoin","litecoin.cash","litecoin.plus","litecoin.ultra","litecred","litedoge","litex","livepeer","lobstex","local.coin.swap","locicoin","lockchain","logiscoin","loki","lomocoin","loom.network","loopring","loopring.neo","loyalcoin","lrm.coin","luna.coin","luna.stars","lunyr","luxcoin","lykke","lympo","machine.xchange.coin","maecenas","maggie","magi","magnum","maidsafecoin","mainframe","mainstream.for.the.underground","maker","mallcoin","manna","mao.zedong","mark.space","martexcoin","masari","massgrid","master.contract.token","master.swiscoin","masternet","matrix.ai.network","matryx","maverick.chain","maxcoin","maximine.coin","mcap","measurable.data.token","medibit","medibloc","medical.chain","mediccoin","medishares","medx","meetone","megacoin","melon","memetic","menlo.one","merculet","mercury","mero","messe.token","metadium","metal","metamorph","metaverse","metronome","mex","mfit.coin","mib.coin","microbitcoin","micromines","micromoney","midasprotocol","milocoin","mincoin","mindexcoin","minereum","miners.reward.token","minex","minexcoin","mintcoin","mir.coin","mirai","mithril","mithril.ore","mixin","mktcoin","mmocoin","mnpcoin","moac","mobilego","mobilinktoken","mobius","model.x.coin","modultrade","modum","moeda.loyalty.points","moin","mojocoin","molecular.future","molecule","monacocoin","monacoin","monero","monero.classic","monetaryunit","monetha","moneytoken","monkey.project","monster.byte","mooncoin","more.coin","morpheus.labs","morpheus.network","moss.coin","motocoin")
m5<-c("moving.cloud.coin","msd","mtc.mesh.network","musicoin","mustangcoin","mvl","mybit","myriad","mysterium","mytoken","mywish","naga","nam.coin","namecoin","nanjcoin","nano","napoleonx","narrative","nasdacoin","nav.coin","naviaddress","ndex","neblio","nebula.ai","nebulas.token","nectar","nekonium","nem","neo","neo.gold","neoscoin","nerva","nerves","netko","netkoin","neumark","neural.protocol","neuro","neurochain","neurotoken","neutron","nevacoin","neverdie","new.power.coin","nework","newstoken","newton.coin.project","newyorkcoin","nexium","nexo","next.exchange","nexty","nexus","nimiq","niobio.cash","niobium.coin","nitro","nix","nkn","no.bs.crypto","noah.coin","noir","noku","nolimitcoin","nos","novacoin","nper","nubits","nucleus.vision","nuggets","nullex","nuls","numeraire","nushares","nxt","nyancoin","nyerium","oax","obitan.chain","obits","obsidian","obxcoin","obyte","oceanchain","oceanlab","octoin.coin","odem","odyssey","ofcoin","okcash","olive","olympic","olympus.labs","omencoin","omisego","omni","omnitude","on.live","ondori","oneledger","oneroot.network","ongsocial","onix","online","ontology","ontology.gas","op.coin","opacity","opal","opcoinx","open.platform","open.trading.network","optimal.shelf.availability.token","optitoken","opus","oraclechain","orbis.token","orbitcoin","ordocoin","origami","origin.sport","origintrail","ormeus.coin","ors.group","ost","otcbtc.token","ourcoin","own","owndata","oxycoin","pabyosi.coin.special","paccoin","pakcoin","pal.network","palletone","pandacoin.pnd","pandemia","paragon","parallelcoin","pareto.rewards","parkbyte","parkgene","parkingo","particl","pascal.coin","patientory","patron","paws.fund","paxos.standard.token","paycent","paycoin2","payday.coin","payfair","paymon","paypex","paypie","pchain","peculium","pecunio","pedity","peepcoin","peercoin","peerplays.ppy","peng","penta","peony","pepe.cash","pesetacoin","petrodollar","phantasma","phantomx","phonecoin","phore","photon","pigeoncoin","piggycoin","pikciochain","pillar","pinkcoin","pirl","pitiscoin","pivx","pixie.coin","pkg.token","plancoin","platincoin","platinumbar","playcoin","playcoin.erc20","playercoin","playgame","playgroundz","playkey","plexcoin","plncoin","pluracoin","plus.coin","plusonecoin","pluton","poa.network","poet","polis","poly.ai","polybius","polymath.network","polyswarm","ponzicoin","popchain","popularcoin","populous","posex","posscoin","postcoin","postoken","posw.coin","potcoin","power.ledger","powercoin","prasm","presearch","president.johnson","president.trump","pressone","primalbase","primas","prime.xi","primecoin","primestone","printex","privatix","privcy","prizm","prochain","procurrency","profile.utility.token","project.coin","project.pai","project.x","promotion.coin","propy","proton.token","proud.money","provoco.token","proxeus","proximax","pumapay","pundi.x","pundi.x.nem","pura","puregold.token","purevidz","purex","putincoin","pylon.network","pyrexcoin","qash","qbao","qbic","qchi","qlink","qtum","quadrantprotocol","quant","quanta.utility.token","quantis.network","quantstamp","quantum.resistant.ledger","quark","quarkchain","quasarcoin","qube","qubitica","quebecoin","quinads","qunqun","quotient","qurito","qwark","qyno","rabbitcoin","radium","ragnarok","raiden.network.token","rapids","rate3","ratecoin","ravencoin","rchain","read","real","realchain","realtract","rebl","record","red","red.pulse","reddcoin","refereum","reftoken")  
m6<-c("regalcoin","relex","remme","ren","renos","rentberry","repme","repo","request","restart.energy.mwat","revain","revolutionvr","rhenium","rightmesh","rimbit","ripio.credit.network","ripple","rise","rivetz","rlc","robet","robotina","rocket.pool","rocketcoin","rookiecoin","rotharium","roulettetoken","rpicoin","rrcoin","rsk.smart.bitcoin","rubex.money","rubies","rublix","rubycoin","ruff","runners","rupaya","rupee","ryo.currency","safe.exchange.coin","safe.trade.coin","safeinsure","sakecoin","sakura.bloom","salpay","salt","salus","santiment","sapien","save.and.gain","savedroid","savenode","scorum.coins","scriv.network","scroll","scryinfo","seal.network","secretcoin","securecoin","seele","seer","segwit2x","selfkey","selfsell","semux","sense","sentinel","sentinel.chain","sentinel.protocol","sequence","sether","shade.token","shadow.token","shard","sharder","sharechain","sharex","sharpay","sharpe.platform.token","shekel","shield.xsh","shift","shinechain","shipchain","shivers","shivom","shopzcoin","show","showhand","shping","siacashcoin","siacoin","sibcoin","sigmacoin","signal.token","signals.network","signatum","silent.notary","simdaq","simmitri","singulardtv","singularitynet","sirin.labs.token","six","sixeleven","skeincoin","skincoin","skrumble.network","skychain","skycoin","skyhub.coin","smart.application.chain","smartcash","smartcoin","smartfox","smartlands","smartmesh","smartofgiving","smartshare","smileycoin","snetwork","snipcoin","snodecoin","snovio","snowgem","soarcoin","social.activity.token","social.lending.token","social.send","socialcoin.socc","sociall","sola.token","solarcoin","solaris","soma","sonder","songcoin","soniq","sonm","sopay","sophiatx","sp8de","spacechain","spankchain","sparkspay","spectre.dividend","spectre.utility","spectrecoin","speed.mining.service","speedcash","spendcoin","sphere","sphere.identity","spindle","sportyco","spreadcoin","sprouts","srcoin","stacs","stakenet","staker","starbase","starchain","starcointv","starta","startcoin","startercoin","stasis.eurs","status","stealth","steem","steem.dollars","steepcoin","stellar","stellite","steneum.coin","stipend","stk","stockchain","storiqa","storj","storjcoin.x","storm","stox","straks","stratis","streamr.datacoin","stronghands","stronghold.token","stronghold.usd","student.coin","substratum","sugar.exchange","sumokoin","suncontract","super.bitcoin","supercoin","superedge","superior.coin","suqa","sureremit","suretly","surety","susd","swarm.city","swarm.fund","swftcoin","swing","swissborg","swisscoin","switcheo","syncfab","syndicate","synereo","synergy","synthetix.network.token","syscoin","taas","tael","tagcoin","tajcoin","talao","target.coin","tcoin","te.food","tekcoin","telcoin","tellurion","teloscoin","tena","tenx","ternio","terracoin","terranova","teslacoin","tether","tezos","tgame","the.abyss","the.champcoin","the.midas.touch.gold","thekey","themis","theresa.may.coin","theta","thingschain","thingsoperatingsystem","thore.cash","thorecoin","thrive.token","thunderstake","ti.value","tidex.token","tierion","tiesdb","tigereum","time.new.bank","timicoin","titcoin","tittiecoin","toacoin","tokenbox","tokencard","tokenclub","tokendesk","tokenomy","tokenpay","tokenstars","tokes","tokia","tokyo","tolar","tomochain","topchain","tourist.token","traceability.chain","tracto","trade.token.x","traid","trakinvest","transcodium","transfercoin","travala","travelflex","travelnote","traxia","trezarcoin","trident","trinity.network.credit","tripio","trittium","trollcoin","tron","tronclassic","truechain","truedeck","trueflip","trueusd")
m7<-c("trumpcoin","trust","trustnote","ttc.protocol","turtlecoin","tv.two","twinkle","typerium","u.network","ubcoin.market","ubex","ubiq","ubique.chain.of.things","ucash","uchain","ugchain","ulord","ultimate.secure.cash","ultra.salescoud","ultracoin","ultranote.coin","unibright","uniform.fiscal.object","unify","unikoin.gold","united.bitcoin","universa","universal.currency","universe","unlimitedip","unobtanium","upfiring","uptoken","uquid.coin","uralscoin","usd.coin","usdcoin","usechain.token","utrum","utrust","uttoken","valuechain","valuecybertoken","valuto","vaperscoin","vechain","vector","veltor","verge","vericoin","veridocglobal","verify","verime","veritaseum","veriumreserve","veros","version","vertcoin","vestchain","vethor.token","vetri","vexanium","vezt","viacoin","vibe","viberate","vice.industry.token","vikkytoken","vinchain","vipstar.coin","virtacoin","visionx","vitae","vite","vites","viuly","vivid.coin","vivo","voisecom","volt","votecoin","vslice","vsportcoin","vsync.vsx","vulcano","w3coin","wabnetwork","wagerr","waletoken","waltonchain","wanchain","wandx","waves","waves.community.token","wavesgo","wax","waykichain","wearesatoshi","webchain","webcoin","well","welltrado","wepower","weshow.token","weth","wetoken","whalecoin","white.standard","whitecoin","wi.coin","wiki.token","wild.beast.block","win.coin","winding.tree","wings","wink","wintoken","wispr","witchain","wixlar","wizbl","women","worldcoin","worldcore","wowbit","wxcoins","wys.token","x.cash","x.coin","x12.coin","x8x.token","xaurum","xceltoken","xchange","xdna","xel","xenon","xgox","xinfin.network","xmax","xmct","xovbank","xpa","xriba","xrt.token","xtrabytes","xtrd","xyo","yee","yeed","yenten","yocoin","yolocash","you.coin","youlive.coin","yoyow","yuan.chain.coin","yuki","zap","zayedcoin","zb","zcash","zclassic","zcoin","zcore","zealium","zebi","zeepin","zeitcoin","zelcash","zen.protocol","zencash","zengold","zennies","zenswap.network.token","zero","zetacoin","zeusnetwork","zeusshield","zilla","zilliqa","zinc","zip","zippie","zmine","zoomba","zozocoin","zper","zrcoin","ztcoin","zurcoin")
fecha_19_op<-fecha_19[c(m1,m2,m3,m4,m5,m6,m7)]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_19_op_ajus<-fecha_19_op
fecha_19_op_ajus<-ajust_dia(fecha_19_op,fecha_19_op_ajus)

#função gera o log do retorno das negociaçoes

fecha_19_return<-fecha_19_op_ajus
fecha_19_return<-log_retrono(fecha_19_op_ajus,fecha_19_return)




#substitui NA e inf por 0

fecha_19_return[sapply(fecha_19_return, is.infinite)] <- 0
fecha_19_return[is.na(fecha_19_return)] <- 0


cor_19<-cor(fecha_19_return)
#Função que cria correlação média
graf_cor_19<-cor_19
media_cor_19<-media_matri(cor_19,media_cor_19)



#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_2019<-graf_signed(graf_cor_19,cor_19,media_cor_19)

#Converte a matriz de correlação em vetor
vet_cor_19<-matrix_vet(cor_19,vet_cor_19)


#cria gráficos com e sem outlieres

box_19<-boxplot(vet_cor_19)
vet_cor_19_so<-vet_cor_19[vet_cor_19>=box_19$stats[1] & vet_cor_19<=box_19$stats[5]]
media_cor_19_so <- mean(vet_cor_19_so)
par(mfrow=c(2,2))


hist(vet_cor_19)
plot(density(vet_cor_19))

hist(vet_cor_19_so)
plot(density(vet_cor_19_so))


#write.csv2(fecha_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2019.csv")

#write.csv2(fecha_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_19_op.csv")
#write.csv2(fecha_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_19_op_ajus.csv")
#write.csv2(fecha_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_19_return.csv")

#write.csv2(cor_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_19.csv")
write.csv2(signed_2019,"C:/Users/georg/Documents/Artigo/Grafo_cripto/bases/grafos/signed_19.csv")
#write.csv2(vet_cor_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_19_so.csv")
#write.csv2(vet_cor_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_19.csv")

#########################################################################################################
#########################################################################################################



#Quebra por ano pega de 2013 até 2019
fecha_13_19<-fechamento
#filtra moedas que tem no mínimo 6 meses)
fecha_13_19_op<-fecha_13_19[65:dim(fecha_13_19)[1],c("bitbar","bitcoin","digitalcoin","feathercoin","freicoin","goldcoin",
                                            "ixcoin","litecoin","mincoin","namecoin","novacoin","peercoin","terracoin","worldcoin")]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_13_19_op_ajus<-fecha_13_19_op
fecha_13_19_op_ajus<-ajust_dia(fecha_13_19_op,fecha_13_19_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_13_19_op_ajus->fecha_13_19_return
fecha_13_19_return<-log_retrono(fecha_13_19_op_ajus,fecha_13_19_return)


#substitui NA e inf por 0

fecha_13_19_return[sapply(fecha_13_19_return, is.infinite)] <- 0
fecha_13_19_return[is.na(fecha_13_19_return)] <- 0


cor_13_19<-cor(fecha_13_19_return)
#Função que cria correlação média
media_cor_13_19<-media_matri(cor_13_19,media_cor_13_19)

graf_cor_13_19<-cor_13_19

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_13_19<-graf_signed(graf_cor_13_19,cor_13_19,media_cor_13_19)



#Converte a matriz de correlação em vetor
vet_cor_13_19<-matrix_vet(cor_13_19,vet_cor_13_19)
#cria gráficos com e sem outlieres

box_13_19<-boxplot(vet_cor_13_19)
vet_cor_13_19_so<-vet_cor_13_19[vet_cor_13_19>=box_13_19$stats[1] & vet_cor_13_19<=box_13_19$stats[5]]
media_cor_13_19_so <- mean(vet_cor_13_19_so)
par(mfrow=c(2,2))


hist(vet_cor_13_19)
plot(density(vet_cor_13_19))

hist(vet_cor_13_19_so)
plot(density(vet_cor_13_19_so))



#write.csv2(fecha_13_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2013_19.csv")

#write.csv2(fecha_13_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_13_19_op.csv")
#write.csv2(fecha_13_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_13_19_op_ajus.csv")
#write.csv2(fecha_13_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_13_19_return.csv")

#write.csv2(cor_13_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_13_19.csv")
#write.csv2(signed_13_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_13_19.csv")
#write.csv2(vet_cor_13_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_13_19_so.csv")
#write.csv2(vet_cor_13_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_13_19.csv")

#########################################################################################################
#########################################################################################################



#Quebra por ano pega de 2014 até 2019
fecha_14_19<-filter(fechamento,year(date)!=c(2013))
#filtra moedas que tem no mínimo 6 meses)
fecha_14_19_op<-fecha_14_19[,c("X42.coin","anoncoin","argentum","betacoin","bitbar","bitcoin","casinocoin","datacoin","deutsche.emark","diamond","digitalcoin","dogecoin","feathercoin","fedoracoin","flo","freicoin","goldcoin","infinitecoin","ixcoin","joulecoin","litecoin","megacoin","mooncoin","namecoin","novacoin","nxt","omni","peercoin","primecoin","quark","ripple","sexcoin","tagcoin","terracoin","unobtanium","worldcoin","zetacoin")]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_14_19_op_ajus<-fecha_14_19_op
fecha_14_19_op_ajus<-ajust_dia(fecha_14_19_op,fecha_14_19_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_14_19_op_ajus->fecha_14_19_return
fecha_14_19_return<-log_retrono(fecha_14_19_op_ajus,fecha_14_19_return)


#substitui NA e inf por 0

fecha_14_19_return[sapply(fecha_14_19_return, is.infinite)] <- 0
fecha_14_19_return[is.na(fecha_14_19_return)] <- 0


cor_14_19<-cor(fecha_14_19_return)
#Função que cria correlação média
media_cor_14_19<-media_matri(cor_14_19,media_cor_14_19)

graf_cor_14_19<-cor_14_19

#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_14_19<-graf_signed(graf_cor_14_19,cor_14_19,media_cor_14_19)



#Converte a matriz de correlação em vetor
vet_cor_14_19<-matrix_vet(cor_14_19,vet_cor_14_19)
#cria gráficos com e sem outlieres

box_14_19<-boxplot(vet_cor_14_19)
min(box_14_19$out)
vet_cor_14_19_so<-vet_cor_14_19[vet_cor_14_19>=box_14_19$stats[1] & vet_cor_14_19<=box_14_19$stats[5]]
media_cor_14_19_so <- mean(vet_cor_14_19_so)

par(mfrow=c(2,2))


hist(vet_cor_14_19)
plot(density(vet_cor_14_19))

hist(vet_cor_14_19_so)
plot(density(vet_cor_14_19_so))


#write.csv2(fecha_14_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2014_19.csv")

#write.csv2(fecha_14_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_14_19_op.csv")
#write.csv2(fecha_14_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_14_19_op_ajus.csv")
#write.csv2(fecha_14_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_14_19_return.csv")

#write.csv2(cor_14_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_14_19.csv")
#write.csv2(signed_14_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_14_19.csv")
#write.csv2(vet_cor_14_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_14_19_so.csv")
#write.csv2(vet_cor_14_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_14_19.csv")
#########################################################################################################
#########################################################################################################



#Quebra por ano pega de 2015 até 2019
fecha_15_19<-filter(fechamento,year(date)>=c(2015))
#filtra moedas que tem no mínimo 6 meses)
fecha_15_19_op<-fecha_15_19[,c("X42.coin","acoin","anoncoin","argentum","artbyte","auroracoin","belacoin","betacoin","bitbar","bitbay","bitcny","bitcoin","bitcoin.scrypt","bitmark","bitshares","bitstar","bitswift","blackcoin","blakecoin","blocknet","bluecoin","boolberry","boostcoin","bunnycoin","burst","bytecoin.bcn","canada.ecoin","cannabiscoin","cashcoin","casinocoin","clams","cloakcoin","coin2.1","counterparty","cryptonite","curecoin","dash","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","dimecoin","dnotes","dogecoin","dopecoin","e.gulden","eccoin","einsteinium","emerald","energycoin","exclusivecoin","faircoin","feathercoin","fedoracoin","flo","foldingcoin","freicoin","gamecredits","gcn.coin","globalboost.y","goldcoin","groestlcoin","gulden","huntercoin","hyperstake","infinitecoin","iocoin","ixcoin","joincoin","joulecoin","litecoin","magi","maidsafecoin","maxcoin","megacoin","mincoin","mintcoin","monacoin","monero","monetaryunit","mooncoin","myriad","namecoin","nav.coin","neoscoin","newyorkcoin","novacoin","nubits","nushares","nxt","nyancoin","okcash","omni","opal","orbitcoin","pandacoin.pnd","paycoin2","peercoin","pesetacoin","photon","piggycoin","pinkcoin","potcoin","prime.xi","primecoin","quark","quotient","rabbitcoin","reddcoin","rimbit","ripple","rubycoin","securecoin","sexcoin","smartcoin","solarcoin","spreadcoin","startcoin","stealth","stellar","storjcoin.x","supercoin","syscoin","tagcoin","tekcoin","terracoin","teslacoin","titcoin","tittiecoin","trollcoin","ubiq","ultracoin","uniform.fiscal.object","unobtanium","verge","vericoin","vertcoin","viacoin","virtacoin","whitecoin","worldcoin","zeitcoin","zetacoin")]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_15_19_op_ajus<-fecha_15_19_op
fecha_15_19_op_ajus<-ajust_dia(fecha_15_19_op,fecha_15_19_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_15_19_op_ajus->fecha_15_19_return
fecha_15_19_return<-log_retrono(fecha_15_19_op_ajus,fecha_15_19_return)


#substitui NA e inf por 0

fecha_15_19_return[sapply(fecha_15_19_return, is.infinite)] <- 0
fecha_15_19_return[is.na(fecha_15_19_return)] <- 0


cor_15_19<-cor(fecha_15_19_return)
#Função que cria correlação média
media_cor_15_19<-media_matri(cor_15_19,media_cor_15_19)

graf_cor_15_19<-cor_15_19
#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_15_19<-graf_signed(graf_cor_15_19,cor_15_19,media_cor_15_19)



#Converte a matriz de correlação em vetor
vet_cor_15_19<-matrix_vet(cor_15_19,vet_cor_15_19)
#cria gráficos com e sem outlieres

box_15_19<-boxplot(vet_cor_15_19)
min(box_15_19$out)
vet_cor_15_19_so<-vet_cor_15_19[vet_cor_15_19>=box_15_19$stats[1] & vet_cor_15_19<=box_15_19$stats[5]]
media_cor_15_19_so <- mean(vet_cor_15_19_so)

par(mfrow=c(2,2))


hist(vet_cor_15_19)
plot(density(vet_cor_15_19))

hist(vet_cor_15_19_so)
plot(density(vet_cor_15_19_so))



#write.csv2(fecha_15_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2015_19.csv")

#write.csv2(fecha_15_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_15_19_op.csv")
#write.csv2(fecha_15_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_15_19_op_ajus.csv")
#write.csv2(fecha_15_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_15_19_return.csv")

#write.csv2(cor_15_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_15_19.csv")
#write.csv2(signed_15_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_15_19.csv")
#write.csv2(vet_cor_15_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_15_19_so.csv")
#write.csv2(vet_cor_15_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_15_19.csv")

#########################################################################################################
#########################################################################################################



#Quebra por ano pega de 2016 até 2019
fecha_16_19<-filter(fechamento,year(date)>=c(2016))
#filtra moedas que tem no mínimo 6 meses)
fecha_16_19_op<-fecha_16_19[,c("X1337coin","X42.coin","X8bit","advanced.internet.blocks","adzcoin","aeon","agoras.tokens","amsterdamcoin","anarchistsprime","anoncoin","artbyte","audiocoin","auroracoin","bata","bean.cash","belacoin","bitbar","bitbay","bitbtc","bitcny","bitcoin","bitcoin.plus","bitcrystals","bitmark","bitquark","bitsend","bitshares","bitstar","bitswift","bitusd","bitzeny","blackcoin","blakecoin","blocknet","bolivarcoin","boolberry","boostcoin","bunnycoin","burst","bytecoin.bcn","canada.ecoin","cannabiscoin","capricoin","casinocoin","circuits.of.value","clams","cloakcoin","clubcoin","coin2.1","counterparty","creditbit","crevacoin","crown","cryptonite","curecoin","dash","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","dimecoin","dnotes","dogecoin","dopecoin","e.gulden","eccoin","einsteinium","emerald","emercoin","energycoin","ethereum","eurocoin","evergreencoin","exclusivecoin","expanse","factom","faircoin","feathercoin","fedoracoin","flo","foldingcoin","fujicoin","gambit","gamecredits","gcn.coin","geocoin","global.currency.reserve","globalboost.y","goldcoin","gridcoin","groestlcoin","guccionecoin","gulden","hempcoin","huntercoin","hyperstake","incakoin","infinitecoin","iocoin","joincoin","leocoin","litecoin","litedoge","magi","maidsafecoin","manna","maxcoin","megacoin","mintcoin","moin","monacoin","monero","monetaryunit","mooncoin","myriad","namecoin","nav.coin","nem","neoscoin","neutron","newyorkcoin","nexus","novacoin","nubits","nushares","nxt","nyancoin","obits","okcash","omni","orbitcoin","paccoin","pakcoin","pandacoin.pnd","parallelcoin","paycoin2","peercoin","pesetacoin","petrodollar","piggycoin","pinkcoin","popularcoin","potcoin","prime.xi","primecoin","pura","quark","quotient","radium","ratecoin","reddcoin","rimbit","ripple","rubycoin","secretcoin","securecoin","sexcoin","shift","siacoin","sibcoin","smileycoin","solarcoin","songcoin","sphere","spreadcoin","startcoin","stealth","stellar","storjcoin.x","swing","synereo","synergy","syscoin","tagcoin","tekcoin","terracoin","teslacoin","tether","titcoin","transfercoin","trollcoin","ubiq","ultracoin","universal.currency","unobtanium","verge","vericoin","vertcoin","viacoin","virtacoin","whitecoin","wild.beast.block","worldcoin","x.coin","xaurum","zeitcoin","zetacoin")]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_16_19_op_ajus<-fecha_16_19_op
fecha_16_19_op_ajus<-ajust_dia(fecha_16_19_op,fecha_16_19_op_ajus)

#função gera o log do retorno das negociaçoes
fecha_16_19_op_ajus->fecha_16_19_return
fecha_16_19_return<-log_retrono(fecha_16_19_op_ajus,fecha_16_19_return)


#substitui NA e inf por 0

fecha_16_19_return[sapply(fecha_16_19_return, is.infinite)] <- 0
fecha_16_19_return[is.na(fecha_16_19_return)] <- 0


cor_16_19<-cor(fecha_16_19_return)
#Função que cria correlação média
media_cor_16_19<-media_matri(cor_16_19,media_cor_16_19)

graf_cor_16_19<-cor_16_19
#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_16_19<-graf_signed(graf_cor_16_19,cor_16_19,media_cor_16_19)




#Converte a matriz de correlação em vetor
vet_cor_16_19<-matrix_vet(cor_16_19,vet_cor_16_19)
#cria gráficos com e sem outlieres

box_16_19<-boxplot(vet_cor_16_19)
min(box_16_19$out)
vet_cor_16_19_so<-vet_cor_16_19[vet_cor_16_19>=box_16_19$stats[1] & vet_cor_16_19<=box_16_19$stats[5]]
media_cor_16_19_so <- mean(vet_cor_16_19_so)

par(mfrow=c(2,2))


hist(vet_cor_16_19)
plot(density(vet_cor_16_19))

hist(vet_cor_16_19_so)
plot(density(vet_cor_16_19_so))



#write.csv2(fecha_16_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2016_19.csv")

#write.csv2(fecha_16_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_16_19_op.csv")
#write.csv2(fecha_16_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_16_19_op_ajus.csv")
#write.csv2(fecha_16_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_16_19_return.csv")

#write.csv2(cor_16_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_16_19.csv")
#write.csv2(signed_16_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_16_19.csv")
#write.csv2(vet_cor_16_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_16_19_so.csv")
#write.csv2(vet_cor_16_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_16_19.csv")
#########################################################################################################
#########################################################################################################



#Quebra por ano pega de 2017 até 2019
fecha_17_19<-filter(fechamento,year(date)>=c(2017))
#filtra moedas que tem no mínimo 6 meses)
fecha_17_19_op<-fecha_17_19[,c("X1337coin","X2give","X42.coin","X808coin","X8bit","aces","acoin","advanced.internet.blocks","adzcoin","aeon","agoras.tokens","allsafe","amsterdamcoin","anarchistsprime","anoncoin","aquariuscoin","arcticcoin","ardor","argentum","artbyte","asiadigicoin","atomic.coin","audiocoin","augur","auroracoin","aurumcoin","avatarcoin","b3coin","bata","bean.cash","belacoin","benjirolls","berncash","bitbar","bitbay","bitbtc","bitcloud","bitcny","bitcoin","bitcoin.21","bitcoin.plus","bitcoin.scrypt","bitcrystals","biteur","bitgold","bitmark","bitquark","bitsend","bitshares","bitsilver","bitstar","bitswift","bitusd","bitzeny","blackcoin","blakecoin","blocknet","bluecoin","bolivarcoin","boostcoin","bowscoin","breakout","breakout.stake","btctalkcoin","bumbacoin","bunnycoin","burst","bytecoin.bcn","c.bit","californium","canada.ecoin","cannabiscoin","capricoin","cashcoin","chesscoin","circuits.of.value","clams","cloakcoin","clubcoin","coin2.1","comet","counterparty","creditbit","crevacoin","crown","cryptocarbon","cryptojacks","cryptonite","curecoin","darcrus","dash","decent","decred","deutsche.emark","diamond","digibyte","digitalcoin","digitalnote","digitalprice","digixdao","dimecoin","dnotes","dogecoin","dopecoin","e.dinar.coin","e.gulden","eccoin","edrcoin","einsteinium","elcoin.el","elementrem","emerald","emercoin","energycoin","espers","eternity","ethereum","ethereum.classic","eurocoin","evergreencoin","evil.coin","exclusivecoin","expanse","factom","faircoin","fargocoin","feathercoin","fedoracoin","first.bitcoin","firstblood","flo","foldingcoin","francs","fujicoin","fuzzballs","gambit","gamecredits","gcn.coin","geocoin","global.currency.reserve","globalboost.y","goldblocks","goldcoin","golem.network.tokens","golos","gridcoin","groestlcoin","guccionecoin","gulden","heat.ledger","hempcoin","hicoin","hodlcoin","huntercoin","hush","hyperstake","ico.openledger","iconomi","incakoin","incent","independent.money.system","infinitecoin","inflationcoin","internet.of.people","iocoin","ion","ixcoin","joincoin","joulecoin","karbo","kobocoin","korecoin","kurrent","lanacoin","leocoin","library.credit","lisk","litecoin","litecred","litedoge","lomocoin","luna.coin","lykke","magi","maidsafecoin","manna","martexcoin","maxcoin","megacoin","memetic","mintcoin","moin","mojocoin","monacoin","monero","monetaryunit","mooncoin","motocoin","mustangcoin","myriad","namecoin","nav.coin","nem","neo","neoscoin","neutron","nevacoin","newyorkcoin","nexium","nexus","nolimitcoin","novacoin","nubits","nullex","nushares","nxt","nyancoin","obits","obyte","okcash","omni","orbitcoin","pabyosi.coin.special","paccoin","pakcoin","pandacoin.pnd","parallelcoin","parkbyte","pascal.coin","paycoin2","peercoin","pepe.cash","pesetacoin","petrodollar","photon","piggycoin","pinkcoin","pivx","plncoin","pluton","popularcoin","posex","postcoin","posw.coin","potcoin","president.johnson","president.trump","prime.xi","primecoin","pura","purevidz","putincoin","quark","quebecoin","qwark","radium","ratecoin","reddcoin","revolutionvr","rimbit","ripple","rise","rubies","rubycoin","safe.exchange.coin","salus","securecoin","sequence","sexcoin","shift","siacoin","sibcoin","singulardtv","sixeleven","smartcoin","smileycoin","solarcoin","songcoin","spectrecoin","sphere","spreadcoin","startcoin","stealth","steem","steem.dollars","stellar","storjcoin.x","stratis","swing","syndicate","synereo","synergy","syscoin","tagcoin","tajcoin","tekcoin","terracoin","teslacoin","tether","titcoin","tittiecoin","transfercoin","trollcoin","trumpcoin","ubiq","universal.currency","unobtanium","vaperscoin","veltor","verge","vericoin","veriumreserve","veros","vertcoin","viacoin","vslice","waves","whitecoin","wild.beast.block","wings","worldcoin","x.coin","xaurum","yocoin","zayedcoin","zcash","zclassic","zcoin","zeitcoin","zetacoin","zurcoin")]


#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_17_19_op_ajus<-fecha_17_19_op
fecha_17_19_op_ajus<-ajust_dia(fecha_17_19_op,fecha_17_19_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_17_19_op_ajus->fecha_17_19_return
fecha_17_19_return<-log_retrono(fecha_17_19_op_ajus,fecha_17_19_return)


#substitui NA e inf por 0

fecha_17_19_return[sapply(fecha_17_19_return, is.infinite)] <- 0
fecha_17_19_return[is.na(fecha_17_19_return)] <- 0


cor_17_19<-cor(fecha_17_19_return)
#Função que cria correlação média
media_cor_17_19<-media_matri(cor_17_19,media_cor_17_19)

graf_cor_17_19<-cor_17_19
#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_17_19<-graf_signed(graf_cor_17_19,cor_17_19,media_cor_17_19)


#Converte a matriz de correlação em vetor
vet_cor_17_19<-matrix_vet(cor_17_19,vet_cor_17_19)
#cria gráficos com e sem outlieres

box_17_19<-boxplot(vet_cor_17_19)
min(box_17_19$out)
vet_cor_17_19_so<-vet_cor_17_19[vet_cor_17_19>=box_17_19$stats[1] & vet_cor_17_19<=box_17_19$stats[5]]
media_cor_17_19_so <- mean(vet_cor_17_19_so)

par(mfrow=c(2,2))


hist(vet_cor_17_19)
plot(density(vet_cor_17_19))

hist(vet_cor_17_19_so)
plot(density(vet_cor_17_19_so))



#write.csv2(fecha_17_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2017_19.csv")

#write.csv2(fecha_17_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_17_19_op.csv")
#write.csv2(fecha_17_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_17_19_op_ajus.csv")
#write.csv2(fecha_17_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_17_19_return.csv")

#write.csv2(cor_17_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_17_19.csv")
#write.csv2(signed_17_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_17_19.csv")
#write.csv2(vet_cor_17_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_17_19_so.csv")
#write.csv2(vet_cor_17_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_17_19.csv")

#########################################################################################################
#########################################################################################################


#Quebra por ano pega de 2018 até 2019
fecha_18_19<-filter(fechamento,year(date)>=c(2018))
#filtra moedas que tem no mínimo 6 meses)
#filtra moedas que tem no mínimo 6 meses)
m1<-c("X0x","X1337coin","X2give","X42.coin","X808coin","X8bit","ace","achain","acoin","adcoin","adshares","adtoken","advanced.internet.blocks","adx.net","adzcoin","aelf","aeon","aeron","aeternity","agrello.delta","aichain","aidoc","aidos.kuneen","aigang","aion","airswap","alis","allion","allsafe","alqo","amber","amsterdamcoin","anarchistsprime","anoncoin","appcoins","aquariuscoin","aragon","arbit","arbitragect","arcticcoin","ardor","argentum","argus","ark","artbyte","asch","asiadigicoin","atbcoin","atlant","atmos","atn","atomic.coin","attention.token.of.media","audiocoin","augur","auroracoin","aurumcoin","authorship","autonio","aventus","axiom","b2bx","b3coin","bancor","bankcoin","bankex","basic.attention.token","bastonet","bata","bean.cash","belacoin","benjirolls","berncash","betacoin","biblepay","bibox.token","bigup","billionaire.token","binance.coin","bitbar","bitbase","bitbay","bitbtc","bitclave","bitcloud","bitcny","bitcoal","bitcoin","bitcoin.atom","bitcoin.cash","bitcoin.diamond","bitcoin.god","bitcoin.gold","bitcoin.planet","bitcoin.plus","bitcoin.red","bitcoin.scrypt","bitcoinx","bitcoinz","bitcore","bitcrystals","bitdeal","bitdegree","bitdice","biteur","bitgold","bitmark","bitquark","bitqy","bitradio","bitsend","bitshares","bitsilver","bitstar","bitusd","bitvolt","bitzeny","blackcoin","blackmoon","blakecoin","blakestar","blazercoin","blockcat","blockcdn","blockmason","blocknet","blocktix","blockv","bloomtoken","blox","bluecoin","bodhi","bolivarcoin","bonpay","boolberry","boscoin","bottos","bounty0x","bowscoin","brat","bread","breakout","breakout.stake","briacoin","bridgecoin","btcmoon","bulwark","bumbacoin","bunnycoin","burst","buzzcoin","bytecoin.bcn","bytom","c.bit","cabbage","californium","campuscoin","canada.ecoin","cannabiscoin","canyacoin","cappasity","capricoin","carboncoin","cardano","cashcoin","ccore","cdx.network","centurion","cfun","chainlink","change","chesscoin","chips","chronobank","chronologic","cindicator","circuits.of.value","civic","clams","clearpoll","cloakcoin","clubcoin","cobinhood","cofound.it","coimatic.2","coimatic.3","coin2.1","coinlancer","coinonat","coinonatx","colossusxt","comet","comsa.eth","comsa.xem","condensate","content.and.ad.network","coss","counterparty","coupecoin","crave","crea","cream","creditbit","credo","crevacoin","crown","crypto.com","cryptocarbon","cryptojacks","cryptonex","cryptonite","cryptopay","cryptoping","crystal.clear","curecoin","cvcoin","cybermiles","dai","dalecoin","dao.casino","darcrus","dash","databits","datum","decent","decent.bet","decentraland","decision.token","decred","deepbrain.chain","deeponion","delphy","denarius.dnr","dent","dentacoin","desire","deutsche.emark","dew","diamond","digibyte","digital.money.bits","digitalcoin","digitalnote","digitalprice","digixdao","dimcoin","dimecoin","dinastycoin","district0x","dix.asset","dogecoin","domraider","dopecoin","doubloon","dovu","draftcoin","dragonchain","dubaicoin.dbix","dutch.coin","dynamic","dynamic.trading.rights","dynamiccoin","e.dinar.coin","e.gulden","ea.coin","eboostcoin","ebtcnew","eccoin","echolink","ecobit","ecocoin","edgeless","edrcoin","eidoo","einsteinium","elcoin.el","electra","electroneum","elementrem","elixir","ellaism","eltcoin","elysium","emerald","emercoin","emphy","encrypgen","encryptotel","encryptotel.eth","energo","enigma","enjin.coin","eos","equitrader","erc20","ergo","eroscoin","eryllium","espers","eternity","ethbet","ethbits","ethereum","ethereum.blue","ethereum.classic","ethereum.gold","ethereum.lite","ethereumcash","etheroll","etherparty","ethlend","ethos","eventchain","everex","evergreencoin","everus","evil.coin","exchange.union","exclusivecoin","expanse","experience.points","exrnchain","factom","faircoin","fairgame","fargocoin","feathercoin","fedoracoin","fidentiax","filecoin","first.bitcoin","firstblood","firstcoin","flash","flik","flixxo","flo","flypme","foldingcoin","force","francs","freicoin","fujicoin","fujinto","funfair","fuzzballs","galactrum","gambit","game","gamechain","gamecredits","gas","gcn.coin","geertcoin","genaro.network","genesis.vision")
m2<-c("geysercoin","gifto","global.cryptocurrency","global.currency.reserve","globalboost.y","globaltoken","gnosis.gno","gobyte","gold.reward.token","goldblocks","goldcoin","golem.network.tokens","golfcoin","golos","golos.gold","goodomy","gravitycoin","grid","gridcoin","grimcoin","groestlcoin","guccionecoin","gulden","guppy","gxchain","hacken","happycoin","harmonycoin.hmc","heat.ledger","helleniccoin","hellogold","hempcoin","herocoin","hicoin","high.performance.blockchain","high.voltage","hiveterminal.token","hodlcoin","hollywoodcoin","homeblockcoin","honey","html.coin","humaniq","huntercoin","hush","hyper.pay","hypercash","hyperstake","icon","iconic","iconomi","iethereum","ignis","ignition","incakoin","incent","indorse.token","infinity.economics","inflationcoin","ink","innova","insanecoin.insn","insolar","insurepal","intelligent.trading.foundation","internet.node.token","internet.of.people","internet.of.things","internxt","investfeed","iocoin","ion","iostoken","iot.chain","iota","iquant","irishcoin","iticoin","ixcoin","ixledger","jetcoin","joincoin","joulecoin","karbo","karma","kcash","kekcoin","kickico","kin","kobocoin","kolion","komodo","korecoin","kucoin.shares","kurrent","kyber.network","kz.cash","lamden","lampix","lanacoin","latoken","leocoin","lethean","leverj","library.credit","life","lightning.bitcoin","linda","linx","lisk","litebitcoin","litecoin","litecoin.plus","litecoin.ultra","litedoge","lockchain","lomocoin","loopring","luna.coin","lunyr","luxcoin","maecenas","magi","maidsafecoin","maker","manna","mao.zedong","martexcoin","master.swiscoin","matryx","maverick.chain","maxcoin","mcap","measurable.data.token","medibloc","medishares","melon","memetic","mercury","metal","metaverse","micromoney","mincoin","minereum","miners.reward.token","minex","minexcoin","mintcoin","mobilego","modum","moeda.loyalty.points","moin","mojocoin","monacocoin","monacoin","monero","monetaryunit","monetha","monkey.project","mooncoin","more.coin","motocoin","msd","musicoin","mustangcoin","mybit","myriad","mysterium","mywish","naga","namecoin","nano","nav.coin","neblio","nebulas.token","nekonium","nem","neo","neoscoin","netko","neumark","neuro","neutron","nevacoin","neverdie","newyorkcoin","nexium","nexus","nitro","nolimitcoin","novacoin","nubits","nullex","nuls","numeraire","nushares","nxt","nyancoin","oax","obits","obsidian","obyte","oceanlab","okcash","olympus.labs","omisego","omni","oneroot.network","ongsocial","onix","op.coin","opal","open.trading.network","opus","oraclechain","orbitcoin","ormeus.coin","ost","pabyosi.coin.special","paccoin","pakcoin","pandacoin.pnd","paragon","parallelcoin","particl","pascal.coin","patientory","paycoin2","payfair","paypie","peepcoin","peercoin","peerplays.ppy","pepe.cash","pesetacoin","petrodollar","phantomx","phore","piggycoin","pillar","pinkcoin","piplcoin","pirl","pivx","platinumbar","playercoin","playkey","pluton","poet","polis","poly.ai","polybius","popularcoin","populous","postcoin","posw.coin","potcoin","power.ledger","presearch","president.johnson","president.trump","primalbase","primas","prime.xi","primecoin","privatix","prizm","prochain","procurrency","profile.utility.token","project.x","propy","proud.money","pura","purevidz","putincoin","pylon.network","qash","qbao","qlink","qtum","quantstamp","quantum.resistant.ledger","quark","qube","quebecoin","qunqun","quotient","qwark","rabbitcoin","radium","raiden.network.token","ratecoin","rchain","real","realchain","rebl","red.pulse","reddcoin","regalcoin","renos","request","revain","revolutionvr","rimbit","ripio.credit.network","ripple","rise","rivetz","rlc","roulettetoken","rubies","rubycoin","runners","rupaya","rupee","safe.exchange.coin","safe.trade.coin","salt","salus","santiment","save.and.gain","securecoin","segwit2x","selfkey","sequence","shadow.token","sharechain","shield.xsh","shift","show","siacoin","sibcoin","singulardtv","singularitynet","sirin.labs.token","sixeleven","skeincoin","skincoin","skycoin","smartcash","smartcoin","smartmesh","smileycoin","snovio","soarcoin","social.send","socialcoin.socc","sociall")
m3<-c("solarcoin","solaris","soma","songcoin","sonm","sophiatx","spacechain","spankchain","spectre.utility","spectrecoin","speedcash","sphere","sphere.identity","sportyco","spreadcoin","sprouts","starta","startcoin","status","stealth","steem","steem.dollars","stellar","steneum.coin","storj","storjcoin.x","storm","stox","straks","stratis","streamr.datacoin","stronghands","student.coin","substratum","sugar.exchange","sumokoin","suncontract","super.bitcoin","supercoin","suretly","swarm.city","swftcoin","swing","swisscoin","syndicate","synereo","syscoin","taas","tael","tagcoin","tajcoin","target.coin","tekcoin","telcoin","tellurion","tenx","terracoin","terranova","teslacoin","tether","tezos","the.champcoin","theresa.may.coin","theta","tierion","tiesdb","time.new.bank","titcoin","tittiecoin","toacoin","tokencard","tokenclub","tokes","tokyo","topchain","tracto","transfercoin","trezarcoin","trident","trollcoin","tron","trueflip","trumpcoin","trust","ubiq","ugchain","ultimate.secure.cash","ultracoin","uniform.fiscal.object","unify","unikoin.gold","united.bitcoin","universal.currency","universe","unobtanium","upfiring","uquid.coin","utrust","uttoken","veltor","verge","vericoin","verify","veritaseum","veriumreserve","veros","version","vertcoin","vezt","viacoin","vibe","viberate","virtacoin","viuly","vivo","voisecom","votecoin","vslice","vsync.vsx","wagerr","waltonchain","wandx","waves","waves.community.token","wavesgo","wax","waykichain","wearesatoshi","weth","whalecoin","whitecoin","wi.coin","wild.beast.block","win.coin","wings","women","worldcoin","worldcore","x.coin","xaurum","xel","xenon","xgox","xpa","xtrabytes","yenten","yocoin","yoyow","zap","zcash","zclassic","zcoin","zeitcoin","zencash","zengold","zero","zetacoin","zeusshield","zozocoin","zrcoin")

fecha_18_19_op<-fecha_18_19[c(m1,m2,m3)]

#função verifica os dias sem negociaçã e replica o valor do dia anterior
fecha_18_19_op_ajus<-fecha_18_19_op
fecha_18_19_op_ajus<-ajust_dia(fecha_18_19_op,fecha_18_19_op_ajus)


#função gera o log do retorno das negociaçoes
fecha_18_19_op_ajus->fecha_18_19_return
fecha_18_19_return<-log_retrono(fecha_18_19_op_ajus,fecha_18_19_return)


#substitui NA e inf por 0

fecha_18_19_return[sapply(fecha_18_19_return, is.infinite)] <- 0
fecha_18_19_return[is.na(fecha_18_19_return)] <- 0


cor_18_19<-cor(fecha_18_19_return)
#Função que cria correlação média
media_cor_18_19<-media_matri(cor_18_19,media_cor_18_19)

graf_cor_18_19<-cor_18_19
#Cria matriz 0 e 1 para ografo
#Se correlação for mairo que a méida 1 se não 0
#antes tem q rodar media_matri para pegar a média da matriz triangular media_cor_xx
signed_18_19<-graf_signed(graf_cor_18_19,cor_18_19,media_cor_18_19)


#Converte a matriz de correlação em vetor
vet_cor_18_19<-matrix_vet(cor_18_19,vet_cor_18_19)
#cria gráficos com e sem outlieres

box_18_19<-boxplot(vet_cor_18_19)
min(box_18_19$out)
vet_cor_18_19_so<-vet_cor_18_19[vet_cor_18_19>=box_18_19$stats[1] & vet_cor_18_19<=box_18_19$stats[5]]
media_cor_18_19_so <- mean(vet_cor_18_19_so)

par(mfrow=c(2,2))


hist(vet_cor_18_19)
plot(density(vet_cor_18_19))

hist(vet_cor_18_19_so)
plot(density(vet_cor_18_19_so))

ggdensity(vet_cor_18_19_so, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")

ggqqplot(vet_cor_18_19_so)
qqPlot(vet_cor_18_19_so)



#write.csv2(fecha_18_19,"C:/Users/georg/Documents/Artigo/Nova pasta/fecha_2018_19.csv")

#write.csv2(fecha_18_19_op,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_ajustada/fecha_18_19_op.csv")
#write.csv2(fecha_18_19_op_ajus,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_original/fecha_18_19_op_ajus.csv")
#write.csv2(fecha_18_19_return,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Dados/Base_retorno/fecha_18_19_return.csv")

#write.csv2(cor_18_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/cor_18_19.csv")
#write.csv2(signed_18_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/signed_18_19.csv")
#write.csv2(vet_cor_18_19_so,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_18_19_so.csv")
#write.csv2(vet_cor_18_19,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/Cor_vet/vet_cor_18_19.csv")




#########################################################################################################

#cria vetores de corelação
cor_mat<-matrix(2,13,8)

#Quantidade de moedas por ano 
cor_mat[1,1]<-dim(cor_13)[1]
cor_mat[2,1]<-dim(cor_14)[1]
cor_mat[3,1]<-dim(cor_15)[1]
cor_mat[4,1]<-dim(cor_16)[1]
cor_mat[5,1]<-dim(cor_17)[1]
cor_mat[6,1]<-dim(cor_18)[1]
cor_mat[7,1]<-dim(cor_19)[1]
cor_mat[8,1]<-dim(cor_13_19)[1]
cor_mat[9,1]<-dim(cor_14_19)[1]
cor_mat[10,1]<-dim(cor_15_19)[1]
cor_mat[11,1]<-dim(cor_16_19)[1]
cor_mat[12,1]<-dim(cor_17_19)[1]
cor_mat[13,1]<-dim(cor_18_19)[1]

#Quantidade de correlações únicas
cor_mat[1,2]<-length(vet_cor_13)
cor_mat[2,2]<-length(vet_cor_14)
cor_mat[3,2]<-length(vet_cor_15)
cor_mat[4,2]<-length(vet_cor_16)
cor_mat[5,2]<-length(vet_cor_17)
cor_mat[6,2]<-length(vet_cor_18)
cor_mat[7,2]<-length(vet_cor_19)
cor_mat[8,2]<-length(vet_cor_13_19)
cor_mat[9,2]<-length(vet_cor_14_19)
cor_mat[10,2]<-length(vet_cor_15_19)
cor_mat[11,2]<-length(vet_cor_16_19)
cor_mat[12,2]<-length(vet_cor_17_19)
cor_mat[13,2]<-length(vet_cor_18_19)

#média das correlações por ano com outlieres

cor_mat[1,3]<-media_cor_13
cor_mat[2,3]<-media_cor_14
cor_mat[3,3]<-media_cor_15
cor_mat[4,3]<-media_cor_16
cor_mat[5,3]<-media_cor_17
cor_mat[6,3]<-media_cor_18
cor_mat[7,3]<-media_cor_19
cor_mat[8,3]<-media_cor_13_19
cor_mat[9,3]<-media_cor_14_19
cor_mat[10,3]<-media_cor_15_19
cor_mat[11,3]<-media_cor_16
cor_mat[12,3]<-media_cor_17_19
cor_mat[13,3]<-media_cor_18_19

#quantidade de correlações por ano sem outlier
cor_mat[1,4]<-length(vet_cor_13_so)
cor_mat[2,4]<-length(vet_cor_14_so)
cor_mat[3,4]<-length(vet_cor_15_so)
cor_mat[4,4]<-length(vet_cor_16_so)
cor_mat[5,4]<-length(vet_cor_17_so)
cor_mat[6,4]<-length(vet_cor_18_so)
cor_mat[7,4]<-length(vet_cor_19_so)
cor_mat[8,4]<-length(vet_cor_13_19_so)
cor_mat[9,4]<-length(vet_cor_14_19_so)
cor_mat[10,4]<-length(vet_cor_15_19_so)
cor_mat[11,4]<-length(vet_cor_16_19_so)
cor_mat[12,4]<-length(vet_cor_17_19_so)
cor_mat[13,4]<-length(vet_cor_18_19_so)

#média das correlações por ano sem outlier
cor_mat[1,5]<-media_cor_13_so
cor_mat[2,5]<-media_cor_14_so
cor_mat[3,5]<-media_cor_15_so
cor_mat[4,5]<-media_cor_16_so
cor_mat[5,5]<-media_cor_17_so
cor_mat[6,5]<-media_cor_18_so
cor_mat[7,5]<-media_cor_19_so
cor_mat[8,5]<-media_cor_13_19_so
cor_mat[9,5]<-media_cor_14_19_so
cor_mat[10,5]<-media_cor_15_19_so
cor_mat[11,5]<-media_cor_16_19_so
cor_mat[12,5]<-media_cor_17_19_so
cor_mat[13,5]<-media_cor_18_19_so


par(mfrow=c(3,3))
plot(density(vet_cor_13_so), main="Distribuição das Correlações de 2013", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_14_so), main="Distribuição das Correlações de 2014", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_15_so), main="Distribuição das Correlações de 2015", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_16_so), main="Distribuição das Correlações de 2016", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_17_so), main="Distribuição das Correlações de 2017", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_18_so), main="Distribuição das Correlações de 2018", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_19_so), main="Distribuição das Correlações de 2019", xlab="Correlação", ylab="Densidadae")

par(mfrow=c(2,3))
plot(density(vet_cor_13_19_so), main="Distribuição Correlações de 2013 até 2019", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_14_19_so), main="Distribuição Correlações de 2014 até 2019", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_15_19_so), main="Distribuição Correlações de 2015 até 2019", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_16_19_so), main="Distribuição Correlações de 2016 até 2019", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_17_19_so), main="Distribuição Correlações de 2017 até 2019", xlab="Correlação", ylab="Densidadae")
plot(density(vet_cor_18_19_so), main="Distribuição Correlações de 2018 até 2019", xlab="Correlação", ylab="Densidadae")

cor_mat[1,6]<-skewness(vet_cor_13_so)
cor_mat[2,6]<-skewness(vet_cor_14_so)
cor_mat[3,6]<-skewness(vet_cor_15_so)
cor_mat[4,6]<-skewness(vet_cor_16_so)
cor_mat[5,6]<-skewness(vet_cor_17_so)
cor_mat[6,6]<-skewness(vet_cor_18_so)
cor_mat[7,6]<-skewness(vet_cor_19_so)

cor_mat[8,6]<-skewness(vet_cor_13_19_so)
cor_mat[9,6]<-skewness(vet_cor_14_19_so)
cor_mat[10,6]<-skewness(vet_cor_15_19_so)
cor_mat[11,6]<-skewness(vet_cor_16_19_so)
cor_mat[12,6]<-skewness(vet_cor_17_19_so)
cor_mat[13,6]<-skewness(vet_cor_18_19_so)

cor_mat[1,7]<-kurtosis(vet_cor_13_so)
cor_mat[2,7]<-kurtosis(vet_cor_14_so)
cor_mat[3,7]<-kurtosis(vet_cor_15_so)
cor_mat[4,7]<-kurtosis(vet_cor_16_so)
cor_mat[5,7]<-kurtosis(vet_cor_17_so)
cor_mat[6,7]<-kurtosis(vet_cor_18_so)
cor_mat[7,7]<-kurtosis(vet_cor_19_so)


cor_mat[8,7]<-kurtosis(vet_cor_13_19_so)
cor_mat[9,7]<-kurtosis(vet_cor_14_19_so)
cor_mat[10,7]<-kurtosis(vet_cor_15_19_so)
cor_mat[11,7]<-kurtosis(vet_cor_16_19_so)
cor_mat[12,7]<-kurtosis(vet_cor_17_19_so)
cor_mat[13,7]<-kurtosis(vet_cor_18_19_so)



cor_mat[1,8]<-ks.test(vet_cor_13_so,"pnorm",mean(vet_cor_13_so),sd(vet_cor_13_so))$p.value
cor_mat[2,8]<-ks.test(vet_cor_14_so,"pnorm",mean(vet_cor_14_so),sd(vet_cor_14_so))$p.value
cor_mat[3,8]<-ks.test(vet_cor_15_so,"pnorm",mean(vet_cor_15_so),sd(vet_cor_15_so))$p.value
cor_mat[4,8]<-ks.test(vet_cor_16_so,"pnorm",mean(vet_cor_16_so),sd(vet_cor_16_so))$p.value
cor_mat[5,8]<-ks.test(vet_cor_17_so,"pnorm",mean(vet_cor_17_so),sd(vet_cor_17_so))$p.value
cor_mat[6,8]<-ks.test(vet_cor_18_so,"pnorm",mean(vet_cor_18_so),sd(vet_cor_18_so))$p.value
cor_mat[7,8]<-ks.test(vet_cor_19_so,"pnorm",mean(vet_cor_19_so),sd(vet_cor_19_so))$p.value

cor_mat[8,8]<-ks.test(vet_cor_13_19_so,"pnorm",mean(vet_cor_13_19_so),sd(vet_cor_13_19_so))$p.value
cor_mat[9,8]<-ks.test(vet_cor_14_19_so,"pnorm",mean(vet_cor_14_19_so),sd(vet_cor_14_19_so))$p.value
cor_mat[10,8]<-ks.test(vet_cor_15_19_so,"pnorm",mean(vet_cor_15_19_so),sd(vet_cor_15_19_so))$p.value
cor_mat[11,8]<-ks.test(vet_cor_16_19_so,"pnorm",mean(vet_cor_16_19_so),sd(vet_cor_16_19_so))$p.value
cor_mat[12,8]<-ks.test(vet_cor_17_19_so,"pnorm",mean(vet_cor_17_19_so),sd(vet_cor_17_19_so))$p.value
cor_mat[13,8]<-ks.test(vet_cor_18_19_so,"pnorm",mean(vet_cor_18_19_so),sd(vet_cor_18_19_so))$p.value




colnames(cor_mat)<-c("qdt_moedas","num_corr","med_corr","num_corr_sem_out","med_corr_sem_out","Simetria","curtose","ksteste")
  

#write.csv2(cor_mat,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/test_cor_mat.csv")

#################conta a quantidade de relações de cada grafo.

vet_cor_ge<-matrix(0,13,5)



vet_cor_ge[1,1]<-length(vet_cor_13[vet_cor_13<=-mean(vet_cor_13) | vet_cor_13>=mean(vet_cor_13)])
vet_cor_ge[2,1]<-length(vet_cor_14[vet_cor_14<=-mean(vet_cor_14) | vet_cor_14>=mean(vet_cor_14)])
vet_cor_ge[3,1]<-length(vet_cor_15[vet_cor_15<=-mean(vet_cor_15) | vet_cor_15>=mean(vet_cor_15)])
vet_cor_ge[4,1]<-length(vet_cor_16[vet_cor_16<=-mean(vet_cor_16) | vet_cor_16>=mean(vet_cor_16)])
vet_cor_ge[5,1]<-length(vet_cor_17[vet_cor_17<=-mean(vet_cor_17) | vet_cor_17>=mean(vet_cor_17)])
vet_cor_ge[6,1]<-length(vet_cor_18[vet_cor_18<=-mean(vet_cor_18) | vet_cor_18>=mean(vet_cor_18)])
vet_cor_ge[7,1]<-length(vet_cor_19[vet_cor_19<=-mean(vet_cor_19) | vet_cor_19>=mean(vet_cor_19)])


vet_cor_ge[8,1]<-length(vet_cor_13_19[vet_cor_13_19<=-mean(vet_cor_13_19) | vet_cor_13_19>=mean(vet_cor_13_19)])
vet_cor_ge[9,1]<-length(vet_cor_14_19[vet_cor_14_19<=-mean(vet_cor_14_19) | vet_cor_14_19>=mean(vet_cor_14_19)])
vet_cor_ge[10,1]<-length(vet_cor_15_19[vet_cor_15_19<=-mean(vet_cor_15_19) | vet_cor_15_19>=mean(vet_cor_15_19)])
vet_cor_ge[11,1]<-length(vet_cor_16_19[vet_cor_16_19<=-mean(vet_cor_16_19) | vet_cor_16_19>=mean(vet_cor_16_19)])
vet_cor_ge[12,1]<-length(vet_cor_17_19[vet_cor_17_19<=-mean(vet_cor_17_19) | vet_cor_17_19>=mean(vet_cor_17_19)])
vet_cor_ge[13,1]<-length(vet_cor_18_19[vet_cor_18_19<=-mean(vet_cor_18_19) | vet_cor_18_19>=mean(vet_cor_18_19)])




vet_cor_ge[1,2]<-length(vet_cor_13)
vet_cor_ge[2,2]<-length(vet_cor_14)
vet_cor_ge[3,2]<-length(vet_cor_15)
vet_cor_ge[4,2]<-length(vet_cor_16)
vet_cor_ge[5,2]<-length(vet_cor_17)
vet_cor_ge[6,2]<-length(vet_cor_18)
vet_cor_ge[7,2]<-length(vet_cor_19)


vet_cor_ge[8,2]<-length(vet_cor_13_19)
vet_cor_ge[9,2]<-length(vet_cor_14_19)
vet_cor_ge[10,2]<-length(vet_cor_15_19)
vet_cor_ge[11,2]<-length(vet_cor_16_19)
vet_cor_ge[12,2]<-length(vet_cor_17_19)
vet_cor_ge[13,2]<-length(vet_cor_18_19)


vet_cor_ge[1,3]<-length(vet_cor_13[vet_cor_13>=0])
vet_cor_ge[2,3]<-length(vet_cor_14[vet_cor_14>=0])
vet_cor_ge[3,3]<-length(vet_cor_15[vet_cor_15>=0])
vet_cor_ge[4,3]<-length(vet_cor_16[vet_cor_16>=0])
vet_cor_ge[5,3]<-length(vet_cor_17[vet_cor_17>=0])
vet_cor_ge[6,3]<-length(vet_cor_18[vet_cor_18>=0])
vet_cor_ge[7,3]<-length(vet_cor_19[vet_cor_19>=0])


vet_cor_ge[8,3]<-length(vet_cor_13_19[vet_cor_13_19>=0])
vet_cor_ge[9,3]<-length(vet_cor_14_19[vet_cor_14_19>=0])
vet_cor_ge[10,3]<-length(vet_cor_15_19[vet_cor_15_19>=0])
vet_cor_ge[11,3]<-length(vet_cor_16_19[vet_cor_16_19>=0])
vet_cor_ge[12,3]<-length(vet_cor_17_19[vet_cor_17_19>=0])
vet_cor_ge[13,3]<-length(vet_cor_18_19[vet_cor_18_19>=0])


vet_cor_ge[1,4]<-length(vet_cor_13[vet_cor_13<=-mean(vet_cor_13)])
vet_cor_ge[2,4]<-length(vet_cor_14[vet_cor_14<=-mean(vet_cor_14)])
vet_cor_ge[3,4]<-length(vet_cor_15[vet_cor_15<=-mean(vet_cor_15)])
vet_cor_ge[4,4]<-length(vet_cor_16[vet_cor_16<=-mean(vet_cor_16)])
vet_cor_ge[5,4]<-length(vet_cor_17[vet_cor_17<=-mean(vet_cor_17)])
vet_cor_ge[6,4]<-length(vet_cor_18[vet_cor_18<=-mean(vet_cor_18)])
vet_cor_ge[7,4]<-length(vet_cor_19[vet_cor_19<=-mean(vet_cor_19)])


vet_cor_ge[8,4]<-length(vet_cor_13_19[vet_cor_13_19<=-mean(vet_cor_13_19)])
vet_cor_ge[9,4]<-length(vet_cor_14_19[vet_cor_14_19<=-mean(vet_cor_14_19)])
vet_cor_ge[10,4]<-length(vet_cor_15_19[vet_cor_15_19<=-mean(vet_cor_15_19)])
vet_cor_ge[11,4]<-length(vet_cor_16_19[vet_cor_16_19<=-mean(vet_cor_16_19)])
vet_cor_ge[12,4]<-length(vet_cor_17_19[vet_cor_17_19<=-mean(vet_cor_17_19)])
vet_cor_ge[13,4]<-length(vet_cor_18_19[vet_cor_18_19<=-mean(vet_cor_18_19)])




vet_cor_ge[1,5]<-length(vet_cor_13[vet_cor_13>=mean(vet_cor_13)+2*sd(vet_cor_13) | vet_cor_13<= -(mean(vet_cor_13)-2*sd(vet_cor_13))])
vet_cor_ge[2,5]<-length(vet_cor_14[vet_cor_14>=mean(vet_cor_14)+2*sd(vet_cor_14) | vet_cor_14<= -(mean(vet_cor_14)-2*sd(vet_cor_14))])
vet_cor_ge[3,5]<-length(vet_cor_15[vet_cor_15>=mean(vet_cor_15)+2*sd(vet_cor_15) | vet_cor_15<= -(mean(vet_cor_15)-2*sd(vet_cor_15))])
vet_cor_ge[4,5]<-length(vet_cor_16[vet_cor_16>=mean(vet_cor_16)+2*sd(vet_cor_16) | vet_cor_16<= -(mean(vet_cor_16)-2*sd(vet_cor_16))])
vet_cor_ge[5,5]<-length(vet_cor_17[vet_cor_17>=mean(vet_cor_17)+2*sd(vet_cor_17) | vet_cor_17<= -(mean(vet_cor_17)-2*sd(vet_cor_17))])
vet_cor_ge[6,5]<-length(vet_cor_18[vet_cor_18>=mean(vet_cor_18)+2*sd(vet_cor_18) | vet_cor_18<= -(mean(vet_cor_18)-2*sd(vet_cor_18))])
vet_cor_ge[7,5]<-length(vet_cor_19[vet_cor_19>=mean(vet_cor_19)+2*sd(vet_cor_19) | vet_cor_19<= -(mean(vet_cor_19)-2*sd(vet_cor_19))])

vet_cor_ge[8,5]<-length(vet_cor_13_19[vet_cor_13_19>=mean(vet_cor_13_19)+2*sd(vet_cor_13_19) | vet_cor_13_19<= -(mean(vet_cor_13_19)-2*sd(vet_cor_13_19))])
vet_cor_ge[9,5]<-length(vet_cor_14_19[vet_cor_14_19>=mean(vet_cor_14_19)+2*sd(vet_cor_14_19) | vet_cor_14_19<= -(mean(vet_cor_14_19)-2*sd(vet_cor_14_19))])
vet_cor_ge[10,5]<-length(vet_cor_15_19[vet_cor_15_19>=mean(vet_cor_15_19)+2*sd(vet_cor_15_19) | vet_cor_15_19<= -(mean(vet_cor_15_19)-2*sd(vet_cor_15_19))])
vet_cor_ge[11,5]<-length(vet_cor_16_19[vet_cor_16_19>=mean(vet_cor_16_19)+2*sd(vet_cor_16_19) | vet_cor_16_19<= -(mean(vet_cor_16_19)-2*sd(vet_cor_16_19))])
vet_cor_ge[12,5]<-length(vet_cor_17_19[vet_cor_17_19>=mean(vet_cor_17_19)+2*sd(vet_cor_17_19) | vet_cor_17_19<= -(mean(vet_cor_17_19)-2*sd(vet_cor_17_19))])
vet_cor_ge[13,5]<-length(vet_cor_18_19[vet_cor_18_19>=mean(vet_cor_18_19)+2*sd(vet_cor_18_19) | vet_cor_18_19<= -(mean(vet_cor_18_19)-2*sd(vet_cor_18_19))])

colnames(vet_cor_ge)<-c("Sinal","pesos", "correl_posi","correl_men_menos_media", "binario")




#write.csv2(vet_cor_ge,"C:/Users/georg/Documents/Artigo/Nova pasta/bases/correlacoes/contagem_tam_grafo.csv")


#########################################################################################################
  
#########################################################################################################
#########################################################################################################
#########################################################################################################

#########################contruindo grafos com sinal#####################################################################
#########################################################################################################
#########################################################################################################

##grafo com distânica eucliana
eu_graf_cor_13<-1-graf_cor_13^2
##grafo com peso
w_sinal_2013<-graph.adjacency(eu_graf_cor_13,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2013<-mst(w_sinal_2013)
write_graph(mst_2013, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2013.csv", format = c( "pajek"))
#Communty

library(igraph)
eb <- edge.betweenness.community(mst_2015)
plot(eb, mst_2015,layout=layout.davidson.harel(mst_2015) )
plot(eb)


plot(mst_2015,layout=layout.davidson.harel(mst_2015) )


plot(w_sinal_2013)
sinal_2013<-pega_sinal(signed_2013,sinal_2013)
g_sinal_2013 <- graph_from_adjacency_matrix( signed_2013,diag = FALSE,mode =c("max") )
E(g_sinal_2013)$sign<-sinal_2013
gf_sinal_2013<-as_adj_signed(g_sinal_2013)

as_adj_signed(g_sinal_2013)


laplacian_matrix_signed(g_sinal_2013)

ggsigned(g_sinal_2019,edge_cols=c(3,1))
  ########################################################################################################


##grafo com distânica eucliana
eu_graf_cor_14<-1-graf_cor_14^2
##grafo com peso
w_sinal_2014<-graph.adjacency(eu_graf_cor_14,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2014<-mst(w_sinal_2014)
write_graph(mst_2014, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2014.csv", format = c( "pajek"))



plot(mst_2014,layout=layout.davidson.harel(mst_2014),vertex.size=1)


sinal_2014<-pega_sinal(signed_2014,sinal_2014)
g_sinal_2014 <- graph_from_adjacency_matrix( signed_2014,diag = FALSE,mode =c("max") )

E(g_sinal_2014)$sign<-sinal_2014



ggplot() + 
  geom_segment( data=g_sinal_2014, size = 0.5, colour="grey")


########################################################################################################
#transforma sinais neagativos em positivos para não dar erro.Função não entende valores negativos

##grafo com distânica eucliana
eu_graf_cor_15<-1-graf_cor_15^2
##grafo com peso
w_sinal_2015<-graph.adjacency(eu_graf_cor_15,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2015<-mst(w_sinal_2015)
write_graph(mst_2015, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2015.csv", format = c( "pajek"))


plot(w_sinal_2015)
plot(mst_2015,layout=layout.reingold.tilford,edge.width=E(w_sinal_2015)$weigth*2,vertex.size=1)
plot(eumst_2015,layout=layout.reingold.tilford,edge.width=E(w_sinal_2015)$weigth*2,vertex.size=1)


plot.igraph(mst_2015,layout=layout.reingold.tilford,vertex.size=1)

v_signed_2015<-signed_2015
  
v_signed_2015[v_signed_2015==-1]<- 1

sinal_2015<-pega_sinal(v_signed_2015,sinal_2015)
g_sinal_2015 <- graph_from_adjacency_matrix( v_signed_2015,diag = FALSE,mode =c("max") )
E(g_sinal_2015)$sign<-sinal_2015
gf_sinal_2015<-as_adj_signed(g_sinal_2015)

########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_16<-1-graf_cor_16^2
##grafo com peso
w_sinal_2016<-graph.adjacency(eu_graf_cor_16,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2016<-mst(w_sinal_2016)
write_graph(mst_2016, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2016.csv", format = c( "pajek"))


v_signed_2016<-signed_2016
v_signed_2016[v_signed_2016==-1]<- 1

sinal_2016<-pega_sinal(v_signed_2016,sinal_2016)
g_sinal_2016 <- graph_from_adjacency_matrix( v_signed_2016,diag = FALSE,mode =c("max") )
E(g_sinal_2016)$sign<-sinal_2016
gf_sinal_2016<-as_adj_signed(g_sinal_2016)

########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_17<-1-graf_cor_17^2
##grafo com peso
w_sinal_2017<-graph.adjacency(eu_graf_cor_17,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2017<-mst(w_sinal_2017)
write_graph(mst_2017, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2017.csv", format = c( "pajek"))

v_signed_2017[v_signed_2017==-1]<- 1
sinal_2017<-pega_sinal(v_signed_2017,sinal_2017)
g_sinal_2017 <- graph_from_adjacency_matrix( v_signed_2017,diag = FALSE,mode =c("max") )
E(g_sinal_2017)$sign<-sinal_2017
gf_sinal_2017<-as_adj_signed(g_sinal_2017)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_18<-1-graf_cor_18^2
##grafo com peso
w_sinal_2018<-graph.adjacency(eu_graf_cor_18,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2018<-mst(w_sinal_2018)
write_graph(mst_2018, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2018.csv", format = c( "pajek"))

v_signed_2018[v_signed_2018==-1]<- 1
sinal_2018<-pega_sinal(v_signed_2018,sinal_2018)
g_sinal_2018 <- graph_from_adjacency_matrix( v_signed_2018,diag = FALSE,mode =c("max") )
E(g_sinal_2018)$sign<-sinal_2018
gf_sinal_2018<-as_adj_signed(g_sinal_2018)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_19<-1-graf_cor_19^2
##grafo com peso
w_sinal_2019<-graph.adjacency(eu_graf_cor_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2019<-mst(w_sinal_2019)
write_graph(mst_2019, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2019.csv", format = c( "pajek"))

v_signed_2019<-signed_2019

v_signed_2019[v_signed_2019==-1]<- 1
sinal_2019<-pega_sinal(v_signed_2019,sinal_2019)
g_sinal_2019 <- graph_from_adjacency_matrix( v_signed_2019,diag = FALSE,mode =c("max") )
E(g_sinal_2019)$sign<-sinal_2019
gf_sinal_2019<-as_adj_signed(g_sinal_2019)



########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_13_19<-1-graf_cor_13_19^2
##grafo com peso
w_sinal_2013_19<-graph.adjacency(eu_graf_cor_13_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2013_19<-mst(w_sinal_2013_19)
write_graph(mst_2013_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2013_19.csv", format = c( "pajek"))

v_signed_13_19<-signed_13_19

v_signed_13_19[v_signed_13_19==-1]<- 1
sinal_13_19<-pega_sinal(v_signed_13_19,sinal_13_19)
g_sinal_13_19 <- graph_from_adjacency_matrix( v_signed_13_19,diag = FALSE,mode =c("max") )
E(g_sinal_13_19)$sign<-sinal_13_19
gf_sinal_13_19<-as_adj_signed(g_sinal_13_19)

########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_14_19<-1-graf_cor_14_19^2
##grafo com peso
w_sinal_2014_19<-graph.adjacency(eu_graf_cor_14_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2014_19<-mst(w_sinal_2014_19)
write_graph(mst_2014_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2014_19.csv", format = c( "pajek"))


v_signed_14_19<-signed_14_19

v_signed_14_19[v_signed_14_19==-1]<- 1
sinal_14_19<-pega_sinal(v_signed_14_19,sinal_14_19)
g_sinal_14_19 <- graph_from_adjacency_matrix( v_signed_14_19,diag = FALSE,mode =c("max") )
E(g_sinal_14_19)$sign<-sinal_14_19
gf_sinal_14_19<-as_adj_signed(g_sinal_14_19)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_15_19<-1-graf_cor_15_19^2
##grafo com peso
w_sinal_2015_19<-graph.adjacency(eu_graf_cor_15_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2015_19<-mst(w_sinal_2015_19)
write_graph(mst_2015_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2015_19.csv", format = c( "pajek"))

v_signed_15_19<-signed_15_19

v_signed_15_19[v_signed_15_19==-1]<- 1
sinal_15_19<-pega_sinal(v_signed_15_19,sinal_15_19)
g_sinal_15_19 <- graph_from_adjacency_matrix( v_signed_15_19,diag = FALSE,mode =c("max") )
E(g_sinal_15_19)$sign<-sinal_15_19
gf_sinal_15_19<-as_adj_signed(g_sinal_15_19)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_16_19<-1-graf_cor_16_19^2
##grafo com peso
w_sinal_2016_19<-graph.adjacency(eu_graf_cor_16_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2016_19<-mst(w_sinal_2016_19)
write_graph(mst_2016_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2016_19.csv", format = c( "pajek"))


v_signed_16_19<-signed_16_19

v_signed_16_19[v_signed_16_19==-1]<- 1
sinal_16_19<-pega_sinal(v_signed_16_19,sinal_16_19)
g_sinal_16_19 <- graph_from_adjacency_matrix( v_signed_16_19,diag = FALSE,mode =c("max") )
E(g_sinal_16_19)$sign<-sinal_16_19
gf_sinal_16_19<-as_adj_signed(g_sinal_16_19)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_17_19<-1-graf_cor_17_19^2
##grafo com peso
w_sinal_2017_19<-graph.adjacency(eu_graf_cor_17_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2017_19<-mst(w_sinal_2017_19)
write_graph(mst_2017_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2017_19.csv", format = c( "pajek"))

v_signed_17_19<-signed_17_19

v_signed_17_19[v_signed_17_19==-1]<- 1
sinal_17_19<-pega_sinal(v_signed_17_19,sinal_17_19)
g_sinal_17_19 <- graph_from_adjacency_matrix( v_signed_17_19,diag = FALSE,mode =c("max") )
E(g_sinal_17_19)$sign<-sinal_17_19
gf_sinal_17_19<-as_adj_signed(g_sinal_17_19)


########################################################################################################
##grafo com distânica eucliana
eu_graf_cor_18_19<-1-graf_cor_18_19^2
##grafo com peso
w_sinal_2018_19<-graph.adjacency(eu_graf_cor_18_19,diag = FALSE,mode =c("max"),weighted=TRUE)
##mst
mst_2018_19<-mst(w_sinal_2018_19)
write_graph(mst_2018_19, "C:/Users/georg/Documents/Artigo/Nova pasta/bases/grafos/pajek/mst_2018_19.csv", format = c( "pajek"))

v_signed_18_19<-signed_18_19

v_signed_18_19[v_signed_18_19==-1]<- 1
sinal_18_19<-pega_sinal(v_signed_18_19,sinal_18_19)
g_sinal_18_19 <- graph_from_adjacency_matrix( v_signed_18_19,diag = FALSE,mode =c("max") )
E(g_sinal_18_19)$sign<-sinal_18_19
gf_sinal_18_19<-as_adj_signed(g_sinal_18_19)


#####################################Número de vertices e arestas positivo, negativo e aba
#######dendogramas#########################################
dev.off()
par(cex=1)
dist_cor_13<-as.dist(eu_graf_cor_13)
Clt_13<-hclust(dist_cor_13, method="single")
den_13<-as.dendrogram(Clt_13)
den_13 <- color_branches(den_13, k = 3)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_13.png")
plot(den_13)
dev.off()

#######dendogramas#########################################
par(cex=1)
dist_cor_14<-as.dist(eu_graf_cor_14)
Clt_14<-hclust(dist_cor_14, method="single")
den_14<-as.dendrogram(Clt_14)
den_14 <- color_branches(den_14, k = 8)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_14.png")
plot(den_14)
dev.off()

  #######dendogramas#########################################
par(cex=0.3)
dist_cor_15<-as.dist(eu_graf_cor_15)
Clt_15<-hclust(dist_cor_15, method="single")
den_15<-as.dendrogram(Clt_15)
den_15 <- color_branches(den_15, k = 14)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_15.png")
plot(den_15)
dev.off()

#######dendogramas#########################################
par(cex=0.3)
dist_cor_16<-as.dist(eu_graf_cor_16)
Clt_16<-hclust(dist_cor_16, method="single")
den_16<-as.dendrogram(Clt_16)
den_16 <- color_branches(den_16, k = 14)
#png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_16.png")
plot(den_16)
#dev.off()

#######dendogramas#########################################
par(cex=0.3)
dist_cor_17<-as.dist(eu_graf_cor_17)
Clt_17<-hclust(dist_cor_17, method="single")
den_17<-as.dendrogram(Clt_17)
den_17 <- color_branches(den_17, k = 14)
#png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_17.png")
plot(den_17)
#dev.off()


#######dendogramas#########################################
par(cex=0.3)
dist_cor_18<-as.dist(eu_graf_cor_18)
Clt_18<-hclust(dist_cor_18, method="single")
den_18<-as.dendrogram(Clt_18)
den_18 <- color_branches(den_18, k = 14)
#png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_18.png")
plot(den_18)
#dev.off()
#######dendogramas#########################################
par(cex=0.3)
dist_cor_19<-as.dist(eu_graf_cor_19)
Clt_19<-hclust(dist_cor_19, method="single")
den_19<-as.dendrogram(Clt_19)
den_19 <- color_branches(den_19, k = 14)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_19.png")
plot(den_19)
dev.off()
#write.csv2(xxxx,"C:/Users/georg/Documents/Artigo/Grafo_cripto/dist_13.csv")

#######dendogramas#########################################
par(cex=0.3)
dist_cor_13_19<-as.dist(eu_graf_cor_13_19)
Clt_13_19<-hclust(dist_cor_13_19, method="single")
den_13_19<-as.dendrogram(Clt_13_19)
den_13_19 <- color_branches(den_13_19, k = 14)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_13_19.png")
plot(den_13_19)
dev.off()
#######dendogramas#########################################
par(cex=0.3)
dist_cor_14_19<-as.dist(eu_graf_cor_14_19)
Clt_14_19<-hclust(dist_cor_14_19, method="single")
den_14_19<-as.dendrogram(Clt_14_19)
den_14_19 <- color_branches(den_14_19, k = 14)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_14_19.png")
plot(den_14_19)
dev.off()


#######dendogramas#########################################
par(cex=0.3)
dist_cor_15_19<-as.dist(eu_graf_cor_15_19)
Clt_15_19<-hclust(dist_cor_15_19, method="single")
den_15_19<-as.dendrogram(Clt_15_19)
den_15_19 <- color_branches(den_15_19, k = 15)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_15_19.png")
plot(den_15_19)
dev.off()
#######dendogramas#########################################
par(cex=0.3)
dist_cor_16_19<-as.dist(eu_graf_cor_16_19)
Clt_16_19<-hclust(dist_cor_16_19, method="single")
den_16_19<-as.dendrogram(Clt_16_19)
den_16_19 <- color_branches(den_16_19, k = 16)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_16_19.png")
plot(den_16_19)
dev.off()


#######dendogramas#########################################
par(cex=0.3)
dist_cor_17_19<-as.dist(eu_graf_cor_17_19)
Clt_17_19<-hclust(dist_cor_17_19, method="single")
den_17_19<-as.dendrogram(Clt_17_19)
den_17_19 <- color_branches(den_17_19, k = 17)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_17_19.png")
plot(den_17_19)
dev.off()


#######dendogramas#########################################
par(cex=0.3)
dist_cor_18_19<-as.dist(eu_graf_cor_18_19)
Clt_18_19<-hclust(dist_cor_18_19, method="single")
den_18_19<-as.dendrogram(Clt_18_19)
den_18_19 <- color_branches(den_18_19, k = 18)
png(filename="C:/Users/georg/Documents/Artigo/Grafo_cripto/latex/imagens/clus_18_19.png")
plot(den_18_19)
dev.off()

