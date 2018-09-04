library(zoo)
library(tseries)
library(TSA)
library(erer)
library(xlsx)

#Event Studies#
#Novo Mercado#
load("data_erer_nm.rdata")
ev_nm <- evReturn(nm, firm=c('bbas3','embr3','eter3','sbsp3'), event.date=c("2006-06-28","2006-06-05","2005-03-02","2002-04-24"), y.date=c("data"), index=c("ibov"), event.win=30); ev_nm

#N?vel 1#
load("data_erer_n1.rdata")
ev_n1 <- evReturn(n1, firm=c('bbdc4','cmig4','cnfb4','cple6','elet6','fesa4','ggbr4','goau4','inep4','itsa4','rapt4','trpl4','unip6','usim5','vale5'), event.date=c("2001-06-26","2001-10-17","2003-12-19","2008-05-07","2006-09-29","2006-09-29","2011-01-07","2003-06-25","2001-06-26","2003-06-25","2011-03-31","2001-06-26","2001-06-26","2002-09-18","2004-11-24","2007-10-11","2003-12-12"), y.date=c("data"), index=c("ibov"), event.win=30); ev_n1

#N?vel 2#
load("data_erer_n2.rdata")
ev_n2 <- evReturn(n2, firm=c('fjta4','pomo4'), event.date=c("2011-07-07","2002-09-03"), y.date=c("data"), index=c("ibov"), event.win=30); ev_n2

# An?lises de risco
risk_nm <- evRisk(ev_nm, m=30, r.free="cdi"); risk_nm
risk_n1 <- evRisk(ev_n1, m=30, r.free="cdi"); risk_n1
risk_n2 <- evRisk(ev_n2, m=30, r.free="cdi"); risk_n2