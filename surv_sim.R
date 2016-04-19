# simulate different missing data types
##### generic data setup:
set.seed(977) # this makes the simulation exactly reproducible
ni     = 100  # 100 people
nj     =  10  # 10 week study
id     = rep(1:ni, each=nj)
cond   = rep(c("control", "diet"), each=nj*(ni/2))
base   = round(rep(rnorm(ni, mean=250, sd=10), each=nj))
week   = rep(1:nj, times=ni)
y      = round(base + rnorm(ni*nj, mean=0, sd=1))

# MCAR
prop.m = .07  # 7% missingness
mcar   = runif(ni*nj, min=0, max=1)
y.mcar = ifelse(mcar<prop.m, NA, y)  # unrelated to anything
View(cbind(id, week, cond, base, y, y.mcar))

# MAR
y.mar = matrix(y, ncol=nj, nrow=ni, byrow=TRUE)
for(i in 1:ni){
  for(j in 4:nj){
    dif1 = y.mar[i,j-2]-y.mar[i,j-3]
    dif2 = y.mar[i,j-1]-y.mar[i,j-2]
    if(dif1>0 & dif2>0){  # if weight goes up twice, drops out
      y.mar[i,j:nj] = NA;  break
    }
  }
}
y.mar = as.vector(t(y.mar))
View(cbind(id, week, cond, base, y, y.mar))

# NMAR
sort.y = sort(y, decreasing=TRUE)
nmar   = sort.y[ceiling(prop.m*length(y))]
y.nmar = ifelse(y>nmar, NA, y)  # doesn't show up when heavier
View(cbind(id, week, cond, base, y, y.nmar))

require(survival)
install.packages("survsim")
require(survsim)
require(ggplot2)

dist.ev <- "weibull"
anc.ev <- 1
beta0.ev <- 5.268
dist.cens <- "weibull"
anc.cens <- 1
beta0.cens <- 5.368
x <- list(c("bern", 0.3), c("bern", 0.4))
beta <- list(-0.4, -0.25)

##full data
store.coef=matrix(data=NA,nrow=100,ncol=2)
for (i in 1:100) {
  simple.dat <- simple.surv.sim(300, 365, dist.ev, anc.ev, beta0.ev,dist.cens, anc.cens, beta0.cens, , beta, x)
  full.model=coxph(Surv(start,stop,status)~x+x.1, data=simple.dat)
  store.coef[i,1]=full.model$coef[1]
  store.coef[i,2]=full.model$coef[2]
}

plot.model.1=as.data.frame(store.coef[,1])
plot.model.1$coef=1
colnames(plot.model.1)[colnames(plot.model.1)=="store.coef[, 1]"] <- "coef.val"
plot.model.2=as.data.frame(store.coef[,2])
plot.model.2$coef=2
colnames(plot.model.2)[colnames(plot.model.2)=="store.coef[, 2]"] <- "coef.val"
plot.model=rbind(plot.model.1,plot.model.2)
aggregate(plot.model$coef.val,by=list(plot.model$coef),FUN=mean, na.rm=TRUE)
aggregate(plot.model$coef.val,by=list(plot.model$coef),FUN=sd, na.rm=TRUE)

p <- ggplot(plot.model, aes(factor(coef), coef.val))
p + geom_boxplot()


##MCAR
# MCAR
prop.m = .1  # 10% missingness
store.coef=matrix(data=NA,nrow=100,ncol=2)
for (i in 1:100) {
  simple.dat <- simple.surv.sim(300, 365, dist.ev, anc.ev, beta0.ev,dist.cens, anc.cens, beta0.cens, , beta, x)
  mcar   = runif(300, min=0, max=1)
  simple.dat$status = ifelse(mcar<prop.m, NA, simple.dat$status) 
  full.model=coxph(Surv(start,stop,status)~x+x.1, data=simple.dat)
  store.coef[i,1]=full.model$coef[1]
  store.coef[i,2]=full.model$coef[2]
}

plot.model.1=as.data.frame(store.coef[,1])
plot.model.1$coef=1
colnames(plot.model.1)[colnames(plot.model.1)=="store.coef[, 1]"] <- "coef.val"
plot.model.2=as.data.frame(store.coef[,2])
plot.model.2$coef=2
colnames(plot.model.2)[colnames(plot.model.2)=="store.coef[, 2]"] <- "coef.val"
plot.model=rbind(plot.model.1,plot.model.2)
aggregate(plot.model$coef.val,by=list(plot.model$coef),FUN=mean, na.rm=TRUE)
aggregate(plot.model$coef.val,by=list(plot.model$coef),FUN=sd, na.rm=TRUE)
