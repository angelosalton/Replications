## encoding: UTF-8
## Revolution R Open 3.2.2 - snapshot 2015-08-27

library(rugarch)
library(dplyr)
#library(xlsx)
library(xtable)
library(urca)
library(xts)
#library(astsa)
#library(TSA)
library(forecast)
# tslag <- function(x, d=1){
#     x <- as.vector(x)
#     n <- length(x)
#     c(rep(NA,d),x)[1:n]
#   }

#setwd('C:\\Users\\RegisAugusto\\Dropbox\\shared\\Angelo\\Growth_volatility')

# Dados -----------------------
load("data.RData")
data <- data[complete.cases(data$indprod),] # excluindo NAs na var. dependente
data[which(data$infl==0), "infl"] <- NA # log(0) na var infl para Brasil
data[,3:11] <- log(data[,3:11]) # vars. em log
countryid <- levels(data$country)

# dummy crise 2008: dez-2007 - jun.2009, fonte NBER Business Cycles Dating Commitee
data <- mutate(data, crisis = 0)
data[data$rdate %in% seq.Date(as.Date("2007-12-01"), as.Date("2009-06-01"), by = "month"), "crisis"] <- 1

# nível
series <- split(data, data$country)
for (i in seq(along=series)){
    for(j in 3:12){
        series[[i]][,j] <- ts(series[[i]][,j], end = c(2014,12), frequency=12)
    }
}

# primeira diferença
# series <- split(data, data$country)
# for (i in seq(along=series)){
# 		series[[i]] <- ts(diff(as.matrix(series[[i]][,3:11])), end = c(2014,12), frequency=12)
# }


# gráficos INDPROD
library(ggplot2)

#emergentes
ggplot(data = data[data$country %in% c("Brasil","Chile","Estônia","Índia","México","Rússia","Turquia"),1:3],
	   aes(x = rdate, y = indprod, group = country, colour = country)) + labs(x = "Data", y = "ln(IP_it)") + geom_line() + theme_bw()

# desenvolvidos
ggplot(data = data[data$country %in% c("Alemanha","Canadá","EUA","França","Itália","Japão","Reino Unido"),1:3],
	   aes(x = rdate, y = indprod, group = country, label = country)) + labs(x = "Data", y = "ln(IP_it)") + geom_line() + theme_bw()

# Estatísticas descritivas: 2 dimensões -------------

desc2 <- matrix(NA, length(series), 8)
dimnames(desc2)[[1]] <- countryid
dimnames(desc2)[[2]] <- c("n","média","mediana","desv.pad.","mín.","máx.","início","fim")
meses <- c("jan.","fev.","mar.","abr.","mai.","jun.","jul.","ago.","set.","out.","nov.","dez.")

for(i in seq(along=series)){
		x <- series[[i]][,"indprod"]
		if(any(!is.na(x))==FALSE){
			next
		} else {
			desc2[i,1] <- round(length(na.omit(x)), 4)
			desc2[i,2] <- round(mean(na.omit(x)), 4)
			desc2[i,3] <- round(median(na.omit(x)), 4)
			desc2[i,4] <- round(sd(na.omit(x)), 4)
			desc2[i,5] <- round(min(na.omit(x)), 4)
			desc2[i,6] <- round(max(na.omit(x)), 4)
			desc2[i,7] <- paste(meses[start(na.omit(x))[2]],start(na.omit(x))[1],sep = "/")
			desc2[i,8] <- paste(meses[end(na.omit(x))[2]],end(na.omit(x))[1],sep = "/")
		}
}
rm(x)

# Estatísticas descritivas: 3 dimensões -------------

desc3 <- array(NA, c(9, 8, length(series)))
dimnames(desc3)[[1]] <- colnames(series[[1]])[3:11]
dimnames(desc3)[[2]] <- c("n","média","mediana","desv.pad.","mín.","máx.","início","fim")
dimnames(desc3)[[3]] <- countryid

for(i in seq(along=series)){
    for(j in 1:9){
        x <- series[[i]][,j+2]
        if(any(!is.na(x))==FALSE){
            next
        } else {
            desc3[j,1,i] <- round(length(na.omit(x)), 4)
            desc3[j,2,i] <- round(mean(na.omit(x)), 4)
            desc3[j,3,i] <- round(median(na.omit(x)), 4)
            desc3[j,4,i] <- round(sd(na.omit(x)), 4)
            desc3[j,5,i] <- round(min(na.omit(x)), 4)
            desc3[j,6,i] <- round(max(na.omit(x)), 4)
            desc3[j,7,i] <- paste(meses[start(na.omit(x))[2]],start(na.omit(x))[1],sep = "/")
            desc3[j,8,i] <- paste(meses[end(na.omit(x))[2]],end(na.omit(x))[1],sep = "/")
        }
    }
}
rm(x)

# Matriz de correlações do painel completo
corr <- round(cor(as.matrix(data[,3:11]), use = "complete.obs", method = "spearman"), 4); corr[upper.tri(corr)] <- NA

# Raiz unit?ria --------------

# unitroot <- matrix(NA, 14, 9)
# colnames(unitroot) <- colnames(data)[3:11]; rownames(unitroot) <- countryid
# 
# for(i in seq(along=series)){
#     for(j in 1:9){
#         if(any(!is.na(series[[i]][,j+2]))==FALSE){
#             next
#         } else {
#         unitroot[i,j] <- ur.df(na.omit(series[[i]][,j+2]), type = "trend", selectlags = "AIC")@testreg$coefficients[2,4]
#         }
#     }
# }
# unitroot <- round(unitroot, digits = 4)
# print(xtable(unitroot, digits=4))
# cat("\n","Quais séries são estacionárias? (95%)","\n")
# unitroot<0.05

# Zivot-Andrews unit root with breaks: indprod
unitroot2 <- as.data.frame(matrix(NA, 14, 2)); dimnames(unitroot2) <- list(countryid, c("Teste", "Breakpoint"))
crit.val <- c(-4.93, -4.42, -4.11) # valores críticos do teste, 1,5,10% (são sempre os mesmos)
for(i in seq_along(series)){
	temp <- ur.za(series[[i]]$indprod, model = "trend", lag = 12)
	unitroot2[i,] <- c(as.numeric(format(temp@teststat, digits = 4)), series[[i]][temp@bpoint, 2])
	
	if(as.numeric(unitroot2[i,1]) < crit.val[1]){
		unitroot2[i,1] <- paste0(unitroot2[i,1], "$^{***}$")
	} else if (as.numeric(unitroot2[i,1]) < crit.val[2]){
		unitroot2[i,1] <- paste0(unitroot2[i,1], "$^{**}$")
	} else if (as.numeric(unitroot2[i,1]) < crit.val[3]){
		unitroot2[i,1] <- paste0(unitroot2[i,1], "$^{*}$")
	}
}
unitroot2[,2] <- as.Date(unitroot2[,2]); unitroot2 <- as.matrix(unitroot2)

options(encoding = "UTF-8")
temp <- print(xtable(unitroot2), latex.environments = NULL, floating = F, only.contents = F, sanitize.text.function = identity)
write(temp, file = "Dissertação/urbreak.tex")

# Modelo AR ----------------------------

# Ordens dos processos AR (revisados)
orig_order <- c(4,1,8,7,7,5,6,12,6,3,10,4,4,2); names(orig_order) <- levels(data$country); orig_order

arma <- list()
for (i in seq(along=series)) {
    arma[[i]] <- Arima(series[[i]]$indprod, order = c(orig_order[i],1,0), include.mean=F)
}

# # Outra possibilidade para determinação da ordem: auto.arima
# arma <- list()
# for (i in seq(along=series)) {
# 	arma[[i]] <- auto.arima(diff(series[[i]]$indprod), max.p = 12, max.q = 0,
# 							seasonal = F, stationary = T, stepwise = F, ic = "aic", allowmean = T)
# }


arma.resid <- list()
for (i in seq(along=arma)) {
    arma.resid[[i]] <- residuals(arma[[i]])
}
names(arma) <- countryid; names(arma.resid) <- countryid

# Teste de Ljung-Box nos res?duos do modelo AR
ljung.ar <- lapply(arma.resid, Box.test, type="Ljung-Box", lag=12)
ljung.ar_table <- matrix(NA, length(series), 3)
for (i in seq(along=series)) {
    ljung.ar_table[i,1] <- arma[[i]]$arma[1]
    ljung.ar_table[i,2] <- ljung.ar[[i]]$statistic
    ljung.ar_table[i,3] <- ljung.ar[[i]]$p.value
}
ljung.ar_table <- round(ljung.ar_table, digits=4)
rownames(ljung.ar_table) <- countryid; colnames(ljung.ar_table) <- c("ordem p","qui-quadrado","p-valor");
print(xtable(ljung.ar_table, digits=4)); ljung.ar_table

# Teste LM para presença de efeitos ARCH
require(FinTS)
lapply(arma.resid, ArchTest)

# Modelo GARCH-M ------------------------------

# REGRESSÃO SEM CONTROLES ----------------

# O modelo
spec <- list()
garchm <- list()
garchm_table <- list()
ljung <- list()
ljung2 <- list()
residplot <- list()

for(i in seq(along=series)) {
    spec[[i]]=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, external.regressors=NULL, variance.targeting=FALSE),
              mean.model=list(armaOrder=c(arma[[i]]$arma[1],0), external.regressors=NULL, include.mean=TRUE, archm=TRUE),
              distribution.model = "norm", start.pars=list(), fixed.pars=list())
    garchm[[i]] <- ugarchfit(spec[[i]], as.matrix(diff(series[[i]]$indprod)), out.sample=0)
    garchm_table[[i]] <- garchm[[i]]@fit$matcoef
    ljung[[i]]  <- Box.test(residuals(garchm[[i]]),   lag = 12, type = "Ljung-Box", fitdf = arma[[i]]$arma[1])
    ljung2[[i]] <- Box.test(residuals(garchm[[i]])^2, lag = 12, type = "Ljung-Box", fitdf = 2)
    residplot[[i]] <- density(residuals(garchm[[i]]))
}
names(garchm) <- countryid; names(garchm_table) <- countryid
names(ljung) <- countryid;  names(ljung2) <- countryid

plot(residplot[[1]], main="Distribui??o dos res?duos") # ex.: Alemanha

# ARCH Teste LM
lmtest <- list()
lmtable <- matrix(NA,length(series),2); rownames(lmtable) <- countryid; colnames(lmtable) <- c("qui-quadrado","p-valor")
for (i in seq(along=series)){
	lmtest[[i]] <- rugarch:::.weightedarchlmtest(residuals(garchm[[i]]), sigma(garchm[[i]]), lags=3)
	lmtable[i,1]  <- lmtest[[i]]$statistic
	lmtable[i,2]  <- lmtest[[i]]$p.value
}
lmtable <- round(lmtable, digits = 4); print(xtable(lmtable, digits = 4)); lmtable

# Tabela geral - equa??o da vari?ncia
tabela <- matrix(NA,length(series),15)
for (i in seq(along=series)){
    tabela[i,] <- cbind(garchm_table[[i]]['archm',c(1,2,4)],
                        garchm_table[[i]]['omega',c(1,2,4)],
                        garchm_table[[i]]['alpha1',c(1,2,4)],
                        garchm_table[[i]]['beta1',c(1,2,4)],
                        garchm_table[[i]]['gamma1',c(1,2,4)])
}
tabela <- matrix(sprintf("%1.4f",tabela),length(series),15); tabela <- t(tabela)
colnames(tabela) <- countryid
rownames(tabela) <- c("delta","ep1","p1",
                      "omega","ep2","p2",
                      "alpha","ep3","p3",
                      "beta", "ep4","p4",
                      "gamma","ep5","p5")

for(i in seq(1,15, by = 3)){ # formatação
	for(j in 1:length(series)){
		
		tabela[i+1,j] <- paste0("(",tabela[i+1,j],")")
		
		if(tabela[i+2,j] <= 0.01){
			tabela[i,j] <- paste0(tabela[i,j],"***")
		} else if(tabela[i+2,j] <= 0.05){
			tabela[i,j] <- paste0(tabela[i,j],"**")
		} else if(tabela[i+2,j] <= 0.10){
			tabela[i,j] <- paste0(tabela[i,j],"*")
		}
	}
}
tabela <- tabela[-c(seq(3,15,by = 3)),]
# desenvolvidos
print(xtable(prettyNum(tabela[,c("Alemanha","Canadá","EUA","França","Itália","Japão","Reino Unido")], decimal.mark = ",")))
# emergentes
print(xtable(prettyNum(tabela[,c("Brasil","Chile","Estônia","Índia","México","Rússia","Turquia")], decimal.mark = ",")))


# # REGRESSÃO COM CONTROLES ---------------------
# cat("---REGRESSÃO COM CONTROLES---")
# 
# # excluir India por falta de dados
# series[["Índia"]] <- NULL
# arma[["Índia"]] <- NULL
# countryid <- countryid[-8]
# orig_order <- orig_order[-8]
# 
# # excluindo NAs em comum entre as vars. y e x's
# exog.mean <- c("gcons","pcons","gfcf","xrate")
# exog.var  <- NULL
# 
# for(i in seq(along=series)){
# 	if(is.null(exog.mean) == TRUE){
# 		break
# 	} else {
# 		series[[i]] <- series[[i]][complete.cases(series[[i]]$indprod, series[[i]][,c(exog.mean,exog.var)]),]
# 	}
# }
# 
# # O modelo
# spec <- list()
# garchm <- list()
# garchm_table <- list()
# ljung <- list()
# ljung2 <- list()
# residplot <- list()
# 
# for(i in seq(along=series)) {
# 	spec[[i]]=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, external.regressors=NULL, variance.targeting=FALSE),
# 						 mean.model=list(armaOrder=c(arma[[i]]$arma[1],0), external.regressors=as.matrix(diff(as.ts(series[[i]][,exog.mean]))), include.mean=TRUE, archm=TRUE),
# 						 distribution.model = "norm", start.pars=list(), fixed.pars=list())
# 	garchm[[i]] <- ugarchfit(spec[[i]], as.matrix(diff(series[[i]]$indprod)), out.sample=0)
# 	garchm_table[[i]] <- garchm[[i]]@fit$matcoef
# 	ljung[[i]]  <- Box.test(residuals(garchm[[i]]),   lag = 12, type = "Ljung-Box", fitdf = arma[[i]]$arma[1])
# 	ljung2[[i]] <- Box.test(residuals(garchm[[i]])^2, lag = 12, type = "Ljung-Box", fitdf = 2)
# 	residplot[[i]] <- density(residuals(garchm[[i]]))
# }
# names(garchm) <- countryid; names(garchm_table) <- countryid
# 
# # ARCH Teste LM
# lmtest <- list()
# lmtable <- matrix(NA,length(series),2); rownames(lmtable) <- countryid; colnames(lmtable) <- c("qui-quadrado","p-valor")
# for (i in seq(along=series)){
# 	lmtest[[i]] <- rugarch:::.weightedarchlmtest(residuals(garchm[[i]]), sigma(garchm[[i]]), lags=3)
# 	lmtable[i,1]  <- lmtest[[i]]$statistic
# 	lmtable[i,2]  <- lmtest[[i]]$p.value
# }
# lmtable <- round(lmtable, digits = 4); print(xtable(lmtable, digits = 4)); lmtable
# 
# # Tabela geral - equa??o da vari?ncia
# tab.ncol <- 15+length(exog.mean)*3
# tabela <- matrix(NA,length(series),tab.ncol)
# for (i in seq(along=series)){
# 	tabela[i,] <- cbind(sapply(paste0("garchm_table[[",i,"]]['mxreg",1:length(exog.mean),"',-3]"), function(x) eval(parse(text=x))),
# 						garchm_table[[i]]['archm', -3],
# 						garchm_table[[i]]['omega', -3],
# 						garchm_table[[i]]['alpha1',-3],
# 						garchm_table[[i]]['beta1', -3],
# 						garchm_table[[i]]['gamma1',-3])
# }
# tabela <- matrix(sprintf("%1.4f",tabela),length(series),tab.ncol); tabela <- t(tabela)
# colnames(tabela) <- countryid
# 
# # gerando rownames
# tab.rownames <- rep(NA,tab.ncol)
# tab.rownames[seq(1, tab.ncol, by=3)] <- c(exog.mean,"delta","omega","alpha","beta","gamma")
# tab.rownames[seq(2, tab.ncol, by=3)] <- paste0("ep",1:(5+length(exog.mean)))
# tab.rownames[seq(3, tab.ncol, by=3)] <- paste0("p",1:(5+length(exog.mean)))
# rownames(tabela) <- tab.rownames
# 
# for(i in seq(1, tab.ncol, by = 3)){ # formatação
# 	for(j in 1:length(series)){
# 		
# 		tabela[i+1,j] <- paste0("(",tabela[i+1,j],")")
# 		
# 		if(tabela[i+2,j] <= 0.01){
# 			tabela[i,j] <- paste0(tabela[i,j],"***")
# 		} else if(tabela[i+2,j] <= 0.05){
# 			tabela[i,j] <- paste0(tabela[i,j],"**")
# 		} else if(tabela[i+2,j] <= 0.10){
# 			tabela[i,j] <- paste0(tabela[i,j],"*")
# 		}
# 	}
# }
# tabela <- tabela[-c(seq(3,tab.ncol,by = 3)),]
# tabela <- cbind(tabela,"-"); colnames(tabela)[14] <- "Índia"
# # desenvolvidos
# print(xtable(prettyNum(tabela[,c("Alemanha","Canadá","EUA","França","Itália","Japão","Reino Unido")], decimal.mark = ",")))
# # emergentes
# print(xtable(prettyNum(tabela[,c("Brasil","Chile","Estônia","Índia","México","Rússia","Turquia")], decimal.mark = ",")))

# DADOS MENSAIS ------------------
#
# macro_codes <- c("BRA", "CAN", "CHL", "DEU", "EST", "FRA", "GBR", "ITA", "JPN", "MEX", "RUS", "TUR", "USA")
# library(Quandl)
# library(zoo)
# Quandl.api_key("hctPp9d5oh4hN342NGG_")
# 
# request_pib2 <- function(countr, fonte = "ocde"){
# 	require(Quandl)
# 	N <- length(countr)
# 	apikey <- "hctPp9d5oh4hN342NGG_"
# 	codes <- list()
# 	codes[[1]] <- paste0("OECD/","MEI_CLI_LORSGPOR_IXOBSA_", countr, "_Q") # gdp
# 	
# 	if(fonte == "ocde"){
# 		codes[[2]] <- paste0("OECD/","KEI_NAEXKP02_", countr, "_GP_Q") # cons. priv
# 		codes[[3]] <- paste0("OECD/","KEI_NAEXKP03_", countr, "_GP_Q") # cons. gov
# 		codes[[4]] <- paste0("OECD/","KEI_NAEXKP04_", countr, "_GP_Q") # gfcf
# 		} else if (fonte == "worldbank"){
# 		# World Bank
# 		codes[[2]] <- paste0("WWDI/", countr, "_NE_CON_PETC_ZS") # cons. priv % gdp
# 		codes[[3]] <- paste0("WWDI/", countr, "_NE_CON_GOVT_ZS") # cons. gov % gdp
# 		codes[[4]] <- paste0("WWDI/", countr, "_NE_GDI_FTOT_ZS") # gfcf
# 	}
# 	# paste0("WWDI/",countr,"_FS_AST_DOMS_GD_ZS") # credit % gdp
# 	# paste0("WWDI/",countr,"_NY_GNS_ICTR_ZS") # poupança % gdp
# 	
# 	data <- rep(list(NULL),N)
# 	names(data) <- countr
# 	
# 	for(i in 1:N){
# 		data[[i]] <- Quandl(c(codes[[1]][i],codes[[2]][i],codes[[3]][i],codes[[4]][i]), type = "zoo", order = "asc", collapse = "monthly", start_date = "1961-01-01", end_date = "2014-12-01", authcode = apikey)
# 		colnames(data[[i]]) <- c("gdp","pcons","gcons","gfcf")
# 	}
# 	data # ainda não está em painel
# }
# dates <- as.yearmon(seq.Date(as.Date("1961-01-01"), as.Date("2011-12-01"), by = "month")) # a sequência mensal de datas
# 
# data.m <- request_pib2(macro_codes, fonte = "worldbank")
# data.m <- lapply(data.m, na.approx, xout = dates) # interpolação trimestral -> mensal
# data.m <- lapply(data.m, na.omit)
# 
# load("pwt.RData") # dados da Penn World Table, vamos extrair os dados de capital humano [Barro, Lee (1991)]
# pwt <- pwt[,c("country","date","hc")]
# pwt <- split(pwt, pwt$country)
# pwt <- lapply(pwt, function (x) x[,names(x) %in% "hc"])
# pwt <- lapply(pwt, zoo, order.by = dates)
# 
# data2 <- list()
# for(i in 1:13){
# 	data2[[i]] <- merge.zoo(data.m[[i]], pwt[[i]], all = TRUE)
# }
# rm(data.m,pwt,i); names(data2) <- macro_codes
# data2 <- lapply(data2, na.omit)
# 
# # problema: rugarch não roda com menos de 100 obs.
# obs <- sapply(data2, nrow)
# 
# # Identificando a ordem AR(p)
# arma <- list()
# for(i in 1:13){
# 	arma[[i]] <- auto.arima(data2[[i]]$"gdp", d = 1, max.q = 0, seasonal = FALSE)
# }
# 
# spec <- list()
# garchm <- list()
# garchm_table <- list()
# ljung <- list()
# ljung2 <- list()
# residplot <- list()
# 
# for(i in which(obs >= 100)) {
# 	spec[[i]]=ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, external.regressors=NULL, variance.targeting=FALSE),
# 						 mean.model=list(armaOrder=c(arma[[i]]$arma[1],0), external.regressors=as.matrix(data2[[i]][-1,-1]), include.mean=TRUE, archm=TRUE),
# 						 distribution.model = "norm", start.pars=list(), fixed.pars=list())
# 	garchm[[i]] <- ugarchfit(spec[[i]], as.matrix(diff(data2[[i]]$gdp)[-1]), out.sample=0)
# 	garchm_table[[i]] <- garchm[[i]]@fit$matcoef
# 	ljung[[i]]  <- Box.test(residuals(garchm[[i]]),   lag = 12, type = "Ljung-Box", fitdf = arma[[i]]$arma[1])
# 	ljung2[[i]] <- Box.test(residuals(garchm[[i]])^2, lag = 12, type = "Ljung-Box", fitdf = 2)
# 	residplot[[i]] <- density(residuals(garchm[[i]]))
# }
# names(garchm_table) <- macro_codes
# garchm_table <- garchm_table[which(obs>=100)]
# sapply(garchm_table, function (x) format(x["archm",]))

# MODELOS EM PAINEL ---------------

# library(plm)
# for(i in seq(along=series)){
# 	series[[i]] <- zoo(series[[i]], order.by = series[[i]]$rdate) # classe 'zoo'
# }
# 
# # medidas de volatilidade
# vol1 <- lapply(series, function (x) rollapply(x[,"indprod"], 12, sd)) # desv. pad. móvel
# vol2 <- lapply(garchm, function (x) as.numeric(sigma(x))) # volatilidade estimada pelo GARCH-M
# 
# 
# for(i in seq(along=series)){
# 	vol2[[i]] <- zoo(vol2[[i]], order.by = index(series[[i]][-1])) # índices de tempo corretos para o merge
# 	series[[i]] <- merge.zoo(series[[i]], vol1[[i]], vol2[[i]])
# 	colnames(series[[i]])[c(13,14)] <- c("rollsd","sigma2")
# }
# names(series) <- countryid
# series <- lapply(series, function (x) x[complete.cases(x),]) # sem NA's
# rm(vol1,vol2)
# 
# # o painel
# library(dplyr)
# temp <- lapply(series, as.data.frame)
# temp <- lapply(temp, tbl_df)
# panel <- bind_rows(temp); rm(temp)
# panel <- arrange(panel, country, rdate)
# panel$country <- as.factor(panel$country)
# panel$rdate <- as.Date.character(panel$rdate)
# panel[,-c(1,2)] <- apply(panel[,-c(1,2)], 2, as.numeric)
# 
# # os modelos
# modelos <- list()
# efeitos <- "individual" # pode ser "individual", "time" ou "twoways"
# f1 <- diff(indprod) ~ crisis + rollsd # sem controles, volatilidade: desv. pad. móvel
# f2 <- diff(indprod) ~ crisis + sigma2 # sem controles, volatilidade: série sigma2_t do GARCH-M com controles
# f3 <- diff(indprod) ~ crisis + diff(pcons) + diff(gcons) + diff(gfcf) + infl + xrate + rollsd # com controles, volatilidade: desv. pad. móvel
# f4 <- diff(indprod) ~ crisis + diff(pcons) + diff(gcons) + diff(gfcf) + infl + xrate + sigma2 # com controles, volatilidade: série sigma2_t do GARCH-M com controles
# 
# # AMOSTRA GERAL
# 
# modelos[[1]] <- plm(f1, model = "within", index = c("country","rdate"), effect = efeitos, data = panel)
# modelos[[2]] <- plm(f2, model = "within", index = c("country","rdate"), effect = efeitos, data = panel)
# modelos[[3]] <- plm(f3, model = "within", index = c("country","rdate"), effect = efeitos, data = panel)
# modelos[[4]] <- plm(f4, model = "within", index = c("country","rdate"), effect = efeitos, data = panel)
# 
# # testes
# # correlação serial nos residuos?
# pbgtest(modelos[[4]]) 
# # efeitos fixos ou pooled OLS?
# pFtest(modelos[[4]], plm(f4, model = "pooling", effect = efeitos, data = panel))
# 
# # PAÍSES EMERGENTES
# 
# panel.emerg <- subset(panel, panel$country %in% c("Brasil","Chile","Estônia","Índia","México","Rússia","Turquia"))
# modelos[[5]] <- plm(f1, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.emerg)
# modelos[[6]] <- plm(f2, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.emerg)
# modelos[[7]] <- plm(f3, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.emerg)
# modelos[[8]] <- plm(f4, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.emerg)
# 
# # testes
# # correlação serial nos residuos?
# pbgtest(modelos[[8]]) 
# # efeitos fixos ou pooled OLS?
# pFtest(plm(f4, model = "within", effect = efeitos, data = panel.emerg),
# 	   modelos[[8]])
# 
# # PAÍSES DESENVOLVIDOS
# 
# panel.desenv <- subset(panel, panel$country %in% c("Alemanha","Canadá","EUA","França","Itália","Japão","Reino Unido"))
# modelos[[9]]  <- plm(f1, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.desenv)
# modelos[[10]] <- plm(f2, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.desenv)
# modelos[[11]] <- plm(f3, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.desenv)
# modelos[[12]] <- plm(f4, model = "pooling", index = c("country","rdate"), effect = efeitos, data = panel.desenv)
# 
# # testes
# # correlação serial nos residuos?
# pbgtest(modelos[[12]]) 
# # efeitos fixos ou pooled OLS?
# pFtest(plm(f4, model = "within", effect = efeitos, data = panel.desenv),
# 	   modelos[[12]])
# 
# # resultados
# lapply(modelos, summary)
# 
# library(stargazer)
# label.table <- c("Crise 2008 (dummy)","Consumo privado","Consumo do governo","FBCF","Taxa de inflação","Taxa de câmbio","Vol. desv.pad.","Vol. GARCH")
# stargazer(modelos[1:12],  type = "latex", df = F, digits = 4, decimal.mark = ",", table.placement = "ht",
# 		  font.size = "small", star.char = c("\\dag","\\ddag","*"), notes = "\\textbf{Fonte:} Elaboração própria. $^{*}$: p<0,01; $^{\\ddag}$: p<0,05; $\\dag$: p<0,10.",
# 		  notes.append = F, dep.var.caption = "", dep.var.labels.include = F, no.space = T,
# 		  label = "tab:resultados3", title = "Resultados dos modelos em painel", covariate.labels = label.table)
