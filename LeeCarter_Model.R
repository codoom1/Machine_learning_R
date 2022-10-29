###Lee Carter Model

ConstrLC=function(ax,bx,kt,b0x,gc,wxt,ages){
  c1=mean(kt[1,],na.rm=TRUE )
  c2=sum(bx[,1], na.rm=TRUE)
  list(ax+c1*bx,bx=bx/c2, kt=c2*(kt-c1))
  
}

library(devtools)

library(StMoMo)
LC=StMoMo(link = "logit",staticAgeFun = TRUE, periodAgeFun = "NP",constFun = ConstrLC)

LC=lc(link="logit")

#####Example with data
Dxt=EWMaleData$Dxt

Ext=EWMaleData$Ext+0.5*EWMaleData$Dxt

Ages=EWMaleData$ages

years=EWMaleData$years

Ages.fit=55:89

wxt=genWeightMat(ages = Ages.fit, years = years,clip=3)

length(Ages.fit)
length(years)
nrow(Dxt)
length(Ages)

nrow(Ext)
#####Fitting the lee carter model
LCfit= fit(LC, Dxt=Dxt, Ext = Ext, ages = Ages, years = years,ages.fit = Ages.fit,wxt = wxt)
summary(LCfit)

LCfit$ax
LCfit$bx
sum(LCfit$bx)
sum(LCfit$kt)

plot(LCfit,nCol = 3)

LCres=residuals(LCfit)

plot(LCres, type="scatter",reslim=c(-3.5,3.5))





###### Forcasting 
LCfor=forecast(LCfit, h=50)
LCfor$rates
LCfor$kt.f

confint(LCfit)

####### Simulating Kt

set.seed(4564)
n=700
LCsimulate=simulate(LCfit, nsim = n,h=50)

LCsimulate$rates
LCsimulate$kt.s




###Plot period Index

plot(LCfit$years,LCfit$kt[1,], xlim = range(LCfit$years,LCsimulate$kt.s$years)
     , ylim = range(LCfit$kt, LCsimulate$kt.s$sim[1, , 1:20]),
     type = "l", xlab = "year", ylab = "kt", main = "Period Index")
matlines(LCsimulate$kt.s$years, LCsimulate$kt.s$sim[1, ,1:20],type = "l",
         lty = 1)



# #Plot cohort index
# plot(LCfit$cohorts, LCfit$gc,
#      xlim = range(LCfit$cohorts, LCsimulate$gc.s$cohorts),
#      ylim = range(LCfit$gc, LCsimulate$gc.s$sim[, 1:20], na.rm = TRUE),
#      type = "l", xlab = "year", ylab = "kt",
#      main = "Cohort index (ARIMA(1,1,0) with drift)")
# matlines(LCsimulate$gc.s$cohorts, LCsimulate$gc.s$sim[, 1:20], type = "l", lty = 1)


#Plot rates at age 65
qxt <- Dxt / Ext
plot(LCfit$years, qxt["65", ], xlim = range(LCfit$years, LCsimulate$years),
     ylim = range(qxt["65", ], LCsimulate$rates["65", , 1:20]), type = "l",
     xlab = "year", ylab = "rate", main = "Mortality rates at age 65")
matlines(LCsimulate$years, LCsimulate$rates["65", , 1:20], type = "l", lty = 1)


library(fanplot)
probs = c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- Dxt / Ext
####Age 65
plot(LCfit$years, qxt["65", ], xlim = c(1960, 2061), ylim = c(0.0025, 0.2),
     xlab = "year", ylab = "mortality rate (log scale)",
     pch = 20, log = "y")


fan(t(LCsimulate$rates["65", , ]), start = 2012, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("black", "white")), ln = NULL)
#age 75
points(LCfit$years, qxt["75", ], pch = 20)
fan(t(LCsimulate$rates["75", , ]), start = 2012, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("red", "white")), ln = NULL)
#age 85
points(LCfit$years, qxt["85", ], pch = 20)
fan(t(LCsimulate$rates["85", , ]), start = 2012, probs = probs, n.fan = 4,
    fan.col = colorRampPalette(c("blue", "white")), ln = NULL)
#labels
text(1965, qxt[c("65", "75", "85"), "1990"],
     labels = c("x = 65", "x = 75", "x = 85"))

library(demography)
NZdata <- hmd.mx(country = "NZL_NP", username = "odoomchristopher22@gmail.com",
                 password = "1648219212")


Ext_NZ <- NZdata$pop$male
Dxt_NZ <- NZdata$rate$male * Ext_NZ
LCfit_NZ <- fit(lc(), Dxt = Dxt_NZ, Ext = Ext_NZ, ages = NZdata$age,
                years = NZdata$year, ages.fit = 0:89, years.fit = 1985:2008)



LCboot_NZ <- bootstrap(LCfit_NZ, nBoot = 50, type = "semiparametric")
plot(LCboot_NZ)

LCsimPU_NZ <- simulate(LCboot_NZ, h = 24)

LCfor_NZ <- forecast(LCfit_NZ, h = 24)
LCsim_NZ <- simulate(LCfit_NZ, nsim = 5000, h = 24)


#Observed, fitted and central forecasts
mxt <- LCfit_NZ$Dxt / LCfit_NZ$Ext
mxtHat <- fitted(LCfit_NZ, type = "rates")
mxtCentral <- LCfor_NZ$rates
#95% Prediction intervals without parameter uncertainty
mxtPred2.5 <- apply(LCsim_NZ$rates, c(1, 2), quantile, probs = 0.025)
mxtPred97.5 <- apply(LCsim_NZ$rates, c(1, 2), quantile, probs = 0.975)
#95% intervals with parameter uncertainty (in sample, and predictions)
mxtHatPU2.5 <- apply(LCsimPU_NZ$fitted, c(1, 2), quantile, probs = 0.025)
mxtHatPU97.5 <- apply(LCsimPU_NZ$fitted, c(1, 2), quantile, probs = 0.975)
mxtPredPU2.5 <- apply(LCsimPU_NZ$rates, c(1, 2), quantile, probs = 0.025)
mxtPredPU97.5 <- apply(LCsimPU_NZ$rates, c(1, 2), quantile, probs = 0.975)
#Plot
x <- c("40", "60", "80")
matplot(LCfit_NZ$years, t(mxt[x, ]),
        xlim = range(LCfit_NZ$years, LCfor_NZ$years),
        ylim = range(mxtHatPU97.5[x, ], mxtPredPU2.5[x, ], mxt[x, ]),
        type = "p", xlab = "years", ylab = "mortality rates (log scale)",
        log = "y", pch = 20, col = "black")
matlines(LCfit_NZ$years, t(mxtHat[x, ]), lty = 1, col = "black")
matlines(LCfit_NZ$years, t(mxtHatPU2.5[x, ]), lty = 5, col = "red")
matlines(LCfit_NZ$years, t(mxtHatPU97.5[x, ]), lty = 5, col = "red")
matlines(LCfor_NZ$years, t(mxtCentral[x, ]), lty = 4, col = "black")
matlines(LCsim_NZ$years, t(mxtPred2.5[x, ]), lty = 3, col = "black")
matlines(LCsim_NZ$years, t(mxtPred97.5[x, ]), lty = 3, col = "black")
matlines(LCsimPU_NZ$years, t(mxtPredPU2.5[x, ]), lty = 5, col = "red")
matlines(LCsimPU_NZ$years, t(mxtPredPU97.5[x, ]), lty = 5, col = "red")
text(1986, mxtHatPU2.5[x, "1995"], labels = c("x=40", "x=60", "x=80"))















###############################
######Simulations

Age=function(a,b){ if (a<b){
  
  
  aa=a:b
  aa
} else { print("Not applicable")}
} 

A=Age(0,103)

year=function(p,q){ if (p<q){
  
  
  pp=p:q
  pp
} else { print("Not applicable")}
} 

B= year(1923,2021)

dxt=function(mi,ma){
  Dxt= matrix(sample(mi:ma, size=length(A)*length(B)), nrow =length(A),ncol=
                length(B))
  
  colnames(Dxt)=B
  rownames(Dxt)=A
  Dxt
  
}
death=dxt(500,10000)   

ext=function(e1,e2){
  
  Ext= matrix(sample(e1:e2, size=length(A)*length(B)), nrow =length(A),ncol=
                length(B))
  
  colnames(Ext)=B
  rownames(Ext)=A
  Ext
  
}

Expo=ext(100000,10000000)
Expo=Expo+0.5*death


# 
#   Mxt= matrix(NA, nrow = length(A), ncol=length(B))
# for(i in 1:length(A)){
#   for(j in 1:length(B)){
#     Mxt[i,j]=log(death[i,j]/Expo[i,j])
#     
#     
#   }
# }
# 
#   
#   
# Mxt
# colnames(Mxt)=B
# rownames(Mxt)=A
# Mxt
# 
# ax= rowMeans(Mxt)
# ax
# plot(ax)
# 
# #### Subtracting the average pattern from each age
# for(i in 1:length(B)){
#   Mxt[i,] <- Mxt[i,] - ax[i]
# 
# 
# }
# Mxt= t(Mxt)
# ####computing bx
# 
# d <- svd(Mxt, 1, 1)
# d$u
# b <- d$v/sum(d$v)
# b
# sum(b)
# ?svd()
# 
# ####computing Kt
# 
# k <- d$u * sum(d$v) * d$d[1]
# 
# sum(k)
# 
# 
# Mxt1= matrix(NA, nrow = length(A), ncol=length(B))
# for(i in 1:length(A)){
#   for(j in 1:length(B)){
#     Mxt1[i,j]=death[i,j]/Expo[i,j]
#     
#     
#   }
# }
# Mxt1
# colnames(Mxt1)=B
# rownames(Mxt1)=A
# Mxt1=t(Mxt1)
#  

Ages1.fit=55:89 

wxt1=genWeightMat(ages = Ages.fit, years = B,clip=3)

#####Fitting the lee carter model
LCfit1= fit(LC, Dxt=death, Ext = Expo, ages = A, years = B,ages.fit = Ages1.fit,wxt = wxt1)
summary(LCfit)

LCfit1$ax
LCfit1$bx
sum(LCfit1$bx)
sum(LCfit1$kt)

plot(LCfit1,nCol = 3)

LCres1=residuals(LCfit1)

plot(LCres1, type="scatter",reslim=c(-3.5,3.5))



###### Forcasting 
LCfor1=forecast(LCfit, h=50)
LCfo1r$rates
LCfor1$kt.f








