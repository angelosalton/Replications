pairs <- function(p, method = "tls", portfolio = 5L, estimation = 300, trading = 100, veclags = 1L, parallel = F)
{
	# NOTE: This can't yet handle NA's in asset prices matrix p
	start <- Sys.time()

	if(parallel==T){
	  require(doSNOW)
	  cl <- makeCluster(4)
	  registerDoSNOW(cl)
	}
	# SETUP -------------------------------
	require(egcm)
	require(tsDyn)
	require(urca)
	
	p <- na.locf(p)
	if(class(p) != "matrix"){
		p <- as.matrix(p) # faster than xts ou zoo objects
	}
	p <- log(p) # taking logs of prices
	n <- nrow(p) # how much observations
	Z <- as.integer(portfolio) # max no. of pairs in portfolio at time t

	we <- estimation # estimates/estimation window in periods
	wt <- trading # trading window in periods

	# estimates and trading windows

	# PAIRS IDENTIFICATION ----------------------
	# unit root tests
	cat("Starting unit root tests...","\n")
	adftest <- list()
	adf     <- matrix(NA,ncol(p),3); rownames(adf) <- colnames(p); colnames(adf) <- c("critical 0.05","tau","stationary?")
	for (i in 1:ncol(p)){
		adftest[[i]] <- ur.df(p[is.na(p[,i])==F,i], type = "trend")
		adf[i,1] <- adftest[[i]]@cval[1,2]
		adf[i,2] <- adftest[[i]]@teststat[1]
		adf[i,3] <- ifelse(adf[i,1]>adf[i,2],TRUE,FALSE)
	}
	rm(adftest)
	
	cat("Discarded assets due to stationariety:", rownames(adf[adf[,3]==TRUE,]), "\n" )
	p <- p[,adf[,3]==F] # discarding stationary series
	
	# PAIRS SELECTION -------------------

	cat("Choosing pairs...","\n")
	cmat <- matrix(NA, ncol(p), ncol(p)); colnames(cmat) <- colnames(p); rownames(cmat) <- colnames(p)
	sharpe.ratio <- matrix(NA, ncol(p), ncol(p))
	pairs <- list()

	# function for calculating sharpe ratios
	sr <- function(y,x){
	  r <- sqrt(252)*mean(diff(y)-diff(x), na.rm = T)/sd(diff(y)-diff(x), na.rm = T)
	  return(r)
	}
	
	# this loop tests for all possible pairs of assets for cointegration. a lower triangular matrix is constructed.
	# very expensive in terms of computational resources, should be optimized
	# method 'tls' searches for simple linear cointegration
	# method 'tvecm' searches for linear threshold cointegration
	
	k <- 1 # counter for estimates windows
	
	for(t in seq(1, n-we-wt, by = wt)){         # trading windows
	    
	  # cointegration test loop
		  for(j in 1:(ncol(p)-1)){
			  for(i in (j+1):ncol(p)){
			  ym <- p[t:(t+we-1), i]
			  xm <- p[t:(t+we-1), j]
				if(method == "tls"){
			    cmat[i, j] <- is.cointegrated(egcm(cbind(ym,xm), p.value = 0.05)) # Engle-Granger test for linear cointegration
				}
				else if(method == "tvecm"){
				  # Hansen-Seo (2002) test for threshold cointegration. Null hypothesis: threshold cointegration
				  # parallel computing methods available, 'hpc' argument
				  cmat[i, j] <- TVECM.HStest(cbind(ym, xm),
				                             ngridTh = 20, lag = veclags, nboot = 10, hpc = ifelse(parallel==T,"foreach","none"))$PvalBoot>0.05
				}
				if(cmat[i, j] == TRUE){
				  sharpe.ratio[i, j] <- sr(ym,xm)
				}
			}
		}
	  
		# list with cointegrating pairs in each estimates window and trading window periods
	  # 't init' and 't final' determine the trading window
		pairs[[k]] <- which(cmat == T, arr.ind = T)
		pairs[[k]] <- cbind(pairs[[k]], NA, t+we, (t+we-1)+wt); colnames(pairs[[k]]) <- c("asset 1","asset 2","sharpe","t init","t final")

		# each pair have an associated sharpe ratio
		for(l in 1:nrow(pairs[[k]])){
			pairs[[k]][l, "sharpe"] <- sharpe.ratio[pairs[[k]][l,"asset 1"], pairs[[k]][l,"asset 2"]]
			rownames(pairs[[k]])[l] <- paste(colnames(p)[pairs[[k]][l, 1]],"-", colnames(p)[pairs[[k]][l, 2]], sep = "")
		}
		
		pairs[[k]] <- pairs[[k]][order(pairs[[k]][ ,"sharpe"], decreasing = T), ] # ordering pairs by decreasing sharpe ratios
		#pairs[[k]] <- pairs[[k]][pairs[[k]][ ,"sharpe"]>0, ] # optional: discard pairs with negative sharpe ratios
		ifelse(nrow(pairs[[k]])>Z, pairs[[k]] <- pairs[[k]][1:Z, ], next) # limiting portfolio size to Z
		
		# estimates window can have no pairs at all, if cointegration and optional sharpe ratio rules are never satisfied
		if(nrow(pairs[[k]])==0){pairs[[k]] <- NULL}
		k <- k+1
	  }

	cat("Finished pairs selection!","\n")
	cat("Total time:", as.numeric(difftime(Sys.time(), start, units = "mins")),"minutes","\n")
	return(pairs)
}
	
trading <- function(p, pairs, method = "tls", cost = 0, delay = 1L, estimation = 300, trading = 100, veclags = 1L, parallel = F){
  
  # SETUP ---------------------------
  start <- Sys.time()
  if(method != "tls" & method != "tvecm"){
    stop("Methods: 'tls' or 'tvecm'")
  }
	
  if(parallel==T){
    require(doSNOW)
    cl <- makeCluster(4)
    registerDoSNOW(cl)
  }
  
  n <- nrow(p) # how much observations
  
  time <- index(p)
  p <- na.locf(p)
  if(class(p) != "matrix"){
    p <- as.matrix(p) # faster than xts ou zoo objects
  }
  p <- log(p) # taking logs of prices
  open <- 2 # parameters for least squares trading
  close <- 0.5 # parameters for least squares trading
  c <- cost # transaction cost, in percentage of the operation
  d <- as.integer(delay) # there migth be an delay (in periods) between spread trigger and trades
  Z <- nrow(pairs[[1]]) # max no. of pairs in portfolio at time t
  
  we <- estimation # estimates/estimation window in periods
  wt <- trading # trading window in periods
  
  #declare variables of performance
  signal <- array(0, dim(p)); colnames(signal) <- colnames(p) # signal matrix
  numtrades <- 0 # initializing counter for no. of trades
  tradelog <- character()
  
  beta <- matrix(NA, n, Z) # this matrix will store estimated optimal trade ratios beta=asset1/asset2
  spread <- matrix(NA, n, Z) # this will store the spreads for each pair (residuals of log(price1)-beta*log(price2))

  
	# From now on, two available methods: simple total least squares (reference) and threshold vector error correction model
	# TLS ESTIMATES ----------------------
	
	if(method == "tls"){
	
	for(k in seq_along(pairs)){		    					# for each trading window
	  cat("Estimates window:", k, "/", length(pairs), "\n")
    for(z in 1:nrow(pairs[[k]])){							# selected pairs
      
		    t <- pairs[[k]][z, 't init']
		    
				y <- pairs[[k]][z, 'asset 1']
				x <- pairs[[k]][z, 'asset 2']
				ym <- p[seq(t-we, t-1), y]
				xm <- p[seq(t-we, t-1), x]
				fit.temp <- princomp(~ ym + xm)		 # total least squares
				
				# present spread are based on estmated (past) betas
				beta[seq(t-we, t-1), z] <- fit.temp$loadings[1, 1]/fit.temp$loadings[2, 1]
				spread[seq(t, t+wt-1), z] <- scale(p[seq(t, t+wt-1), y] - beta[t-we, z]*p[seq(t, t+wt-1), x])
		}
	}

	# TLS TRADING --------------------
	cat("Simulating trades...","\n")

	for(k in seq_along(pairs)){ 	# for each trading window
	  for(z in 1:nrow(pairs[[k]])){							# selected pairs
			for(t in seq(pairs[[k]][z, 't init'], pairs[[k]][z, 't final'], by = 1)){ # periods in each trading window
    
		# indexing assets with respect to price matrix
		y <- pairs[[k]][z, 'asset 1']
		x <- pairs[[k]][z, 'asset 2']
  
		# carry yesterday's signals forward
		signal[t, ] <- signal[t-1, ]
		
		if(is.na(spread[t-d, z]) == T){
			  next
		} else if(spread[t-d, z] > open & signal[t,y] == 0){
			numtrades <- numtrades + 1
			signal[t, c(y,x)] <- c(-1,1)
			tradelog <- c(tradelog, paste("Short", colnames(p)[y], "at", exp(p[t, y]),"and Long", colnames(p)[x], "at", exp(p[t, x]), "on period", time[t]))
			
		}	else if(spread[t-d, z] < (-open) & signal[t,y] == 0){
			numtrades <- numtrades + 1
			signal[t, c(y,x)] <- c(1,-1)
			tradelog <- c(tradelog, paste("Long", colnames(p)[y], "at", exp(p[t, y]), "and Short", colnames(p)[x], "at", exp(p[t, y]), "on period", time[t]))
		}
		#closes position
		else if((spread[t-d, z] < (-close) & signal[t,y] == -1) |
		        (spread[t-d, z] > close & signal[t,y] == 1)){
			signal[t, c(y,x)] <- c(0,0)
			tradelog <- c(tradelog, paste("Closed", colnames(p)[y], "and", colnames(p)[x], "on period", time[t]))
		}
		
		#close all pairs because current trading window is over
		if (t == pairs[[k]][z, 't final']){
		  signal[t,] <- 0
		}
			}}}

	# TVECM estimates ---------------------------------
 
	} else if(method == "tvecm"){

	# the following matrices will store current thresholds
	lower.thr <- matrix(NA, n, Z)
	upper.thr <- matrix(NA, n, Z)

	for(k in seq_along(pairs)){		    					# for each trading window
	  cat("Estimates window:", k, "/", length(pairs), "\n")
	  for(z in 1:nrow(pairs[[k]])){							# selected pairs
	    
	    t <- pairs[[k]][z, 't init']
      
      y <- pairs[[k]][z, 'asset 1']
      x <- pairs[[k]][z, 'asset 2']
      ym <- p[seq(t-we, t-1), y]
      xm <- p[seq(t-we, t-1), x]
      
      #browser()
			fit.temp <- TVECM(cbind(ym, xm), lag = 1, nthresh = 2, trim = 0.01, ngridBeta = 100, ngridTh = 100, plot = F, trace = T)
			lower.thr[seq(t, t+wt-1), z] <- fit.temp$model.specific$Thresh[1]
			upper.thr[seq(t, t+wt-1), z] <- fit.temp$model.specific$Thresh[2]
			
			# present spread are based on estimated (past) betas
			beta[seq(t-we, t-1), z] <- -fit.temp$model.specific$coint[2]
			spread[seq(t, t+wt-1), z] <- p[seq(t, t+wt-1), y] - beta[t-we, z]*p[seq(t, t+wt-1), x]
		}
	}

	# TVECM TRADING -----------------------------------
	cat("Simulating trades...","\n")

	for(k in seq_along(pairs)){ 	# for each trading window
	  for(z in 1:nrow(pairs[[k]])){					# selected pairs in each window
	    for(t in seq(pairs[[k]][z, 't init'], pairs[[k]][z, 't final'], by = 1)){ # periods in each trading window

			y <- pairs[[k]][z,'asset 1']
			x <- pairs[[k]][z,'asset 2']
			
			# carry yesterday's signals forward
			signal[t, ] <- signal[t-1, ]

			if(is.na(spread[t-d, z]) == T)
			{
				next
			}
			else if(spread[t-d, z] > upper.thr[t, z] & signal[t,y] == 0)
			{
				numtrades <- numtrades + 1
				signal[t, c(y,x)] <- c(-1,1)
				tradelog <- c(tradelog, paste("Short", colnames(p)[y], "at", exp(p[t, y]),"and Long", colnames(p)[x], "at", exp(p[t, x]), "on period", time[t]))
			}
			else if(spread[t-d, z] < lower.thr[t, z] & signal[t,y] == 0)
			{
				numtrades <- numtrades + 1
				signal[t, c(y,x)] <- c(1,-1)
				tradelog <- c(tradelog, paste("Long", colnames(p)[y], "at", exp(p[t, y]), "and Short", colnames(p)[x], "at", exp(p[t, y]), "on period", time[t]))
			}
			#closes position
			else if((spread[t-d, z] < upper.thr[t, z] & signal[t,y] == -1) |
			        (spread[t-d, z] > lower.thr[t, z] & signal[t,y] ==  1))
			{
				signal[t, c(y,x)] <- c(0,0)
				tradelog <- c(tradelog, paste("Closed", colnames(p)[y], "and", colnames(p), "on period", time[t]))
			}
			
			#close all pairs because current trading window is over
			if (t == pairs[[k]][z, 't final']){
			  signal[t,] <- 0
			}
		}}}
	}

  spread <- spread[1:n,]
  beta <- na.locf(beta)
  
	# carry previous day signals forward
	signal <- na.locf(signal)
	
	# --------------------------------------------------------
	
	# continuously compouned returns
	R <- diff(p)*signal[-1, ]
	
	# the second term represent trading costs
	series.pairs <- cumsum(rowSums(R, na.rm = T)) + numtrades*log((1-c)/(1+c))
	
	# adding time indexes for easy graphing
	series.pairs <- as.zoo(series.pairs); index(series.pairs) <- time[-1]
	ret.pairs <- as.numeric(series.pairs)[n-1]

	# annualize?
	#ret.pairs <- (252*ret.pairs/n)*100

	#results
	results <- c(round(ret.pairs, 4), numtrades)
	names(results) <- c("pairs returns %","operations")
	output <- list(series.pairs, tradelog, results); names(output) <- c("returns","log","results")
	cat("Finished trades!","\n")
	cat("Total time:", as.numeric(difftime(Sys.time(), start, units = "mins")),"minutes","\n")
	return(output)
}
