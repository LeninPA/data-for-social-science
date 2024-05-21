# Funciones anteriores
source("./bootstrap/functions.R")
  
# Carga csv de datos de México a memoria
data_mex <- read.csv('./bootstrap/inputs/SuicideData_Mexico.csv')

# ------------------------------
# Inicio bootstrap
# ------------------------------

# Preparando corrida

# Manera de generar números aleatorios
RNGkind("L'Ecuyer-CMRG") 
# Semilla
set.seed(1)
# Núcleos a utilizar
n <- detectCores() - 1
# Preparación de datos
cc <- unique(data_mex$id)

# Ejecución bootstraping

s <- system.time({
  out <- mclapply(1:1000, function(i) {
    samp <- data.frame(id=sample(cc,length(cc),replace=T))
    subdata <- inner_join(data_mex,samp)
    summary(felm(rate_adj ~ tmean + prec | id + statemonth + state_year, weights = subdata$popw, data = subdata))$coefficients[c("tmean"),"Estimate"]
  }, mc.cores = n)
})
s
# Convierte out en vector
out <- unlist(out)
# Guarda resultados en CSV
df <- data.frame(x = 1:length(out), est = out)
write_csv(df,path="./bootstrap/inputs/BootstrapMainModel_mex.csv")

# ------------------------------
# Inicio graficación
# ------------------------------

# Tratamiento y ajuste de datos México

data_mex$yearmonth <- data_mex$year*100+data_mex$month
  xx=5:35
modata_mex1 <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)
  yym = data.frame(xx,stateyear=coef(modata_mex1)[1]*xx)
modata_mex2 <- felm(rate_adj ~ tmean + prec | id + statemonth + state_year | 0 | id , data=data_mex)  #no weights
  yym = data.frame(yym,noweights=coef(modata_mex2)[1]*xx)
modata_mex3 <- felm(rate_adj ~ tmean + prec | id + statemonth + year | 0 | id , data=data_mex, weights=data_mex$popw)  #year FE
  yym = data.frame(yym,year=coef(modata_mex3)[1]*xx)
modata_mex4 <- felm(rate_adj ~ tmean + prec + as.factor(state)*year | id + statemonth + year | 0 | id , data=data_mex, weights=data_mex$popw)  #state TT
 yym = data.frame(yym,yearTT=coef(modata_mex4)[1]*xx)
modata_mex5 <- felm(rate_adj ~ tmean + prec | id + statemonth + yearmonth | 0 | id , data=data_mex, weights=data_mex$popw)  #year-month
  yym = data.frame(yym,yearmonth=coef(modata_mex5)[1]*xx)
modata_mex6 <- felm(rate_adj ~ poly(tmean,3,raw=T) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
  yym = data.frame(yym,poly=as.numeric(t(as.matrix(coef(modata_mex6)[1:3]))%*%t(matrix(nrow=length(xx),ncol=3,data=poly(xx,3,raw=T)))))
modata_mex7 <- felm(rate_adj ~ ns(tmean,knots=c(10,20,30)) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
  yym = data.frame(yym,spline=as.numeric(t(as.matrix(coef(modata_mex7)[1:4]))%*%t(matrix(nrow=length(xx),ncol=4,data=ns(xx,knots=c(10,20,30))))))
modata_mex8 <- felm(rate_adj ~ ns(tmean,df=8) + prec | id + statemonth + state_year | 0 | id , data=data_mex, weights=data_mex$popw)  #polynomial
  yym = data.frame(yym,spline7=as.numeric(t(as.matrix(coef(modata_mex8)[1:8]))%*%t(matrix(nrow=length(xx),ncol=8,data=ns(xx,df=8)))))

### Graficación figuras ###
  
pdf(file="./outputs/Figura1.pdf",height=8,width=10)
      par(mfrow=c(1,2))
  
  
# Panel de México
xx=5:35
nn <- dim(yym)[2]

#calculate weighted mean for base rate
br <- weighted.mean(data_mex$rate_adj,data_mex$popw,na.rm=T)

#read-in bootstrapped runs from above to get CI
boot <- read.csv("inputs/bootstrap_runs/BootstrapMainModel_mex.csv")
  est <- as.matrix(boot$est)%*%matrix(xx,ncol=length(xx))
  est <- est - est[,which(xx==20)] 
  ci <- apply(est,2,function(x) quantile(x,probs=c(0.025,0.975)))/br*100
  yyr2 <- yym
  for (i in 2:nn) {
        yyr2[,i] <- (yym[,i] - yym[yym$xx==20,i])/br*100
  }
  
#plot response curves + ci 
  plot(1,type="n",las=1,ylim=c(-50,50),xlim=c(0,40))
    polygon(c(xx,rev(xx)),c(ci[1,],rev(ci[2,])),col="lightblue",border = NA)
    lines(xx,yyr2[,2],lwd=2)

abline(h=0,lty=2,col="black",lwd=0.5)

#alternative specs
  clz = c(NA,NA,rep("orange",2),rep("red",2),rep("blue",3))
  lp = c(NA,NA,rep(c(1,2),3),3)

    for (i in 3:nn) {
      lines(xx,yyr2[,i],col=clz[i],lty=lp[i],lwd=2)
    }

#plot hist below
    bb = 40
    ht <- hist(data_mex$tmean,breaks=bb,plot=F)
    bb <- length(ht$breaks)  #hist() doesn't follow your exact instructions sometimes...
    rect(ht$breaks[1:(bb-1)],-50,ht$breaks[2:bb],-50+ht$counts/max(ht$counts)*10,col="lightblue")
    
text(35,yyr2[dim(yyr2)[1],2:nn],names(yyr2)[2:nn],pos=4,cex=0.5)
    
dev.off()

