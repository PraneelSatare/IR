library(MASS)
#Read in the control sites data, and then select the variables used
control <- read.table("C:/Users/sharv/Downloads/control.txt", header = TRUE)
attach(control)
x1 <- Av.Sp
x2 <- X.ov.lim
x3 <- Flow / 10000
y <- Total
x4 <- as.numeric(Road.Class == 1)
x5 <- as.numeric(Road.Class == 2)
x6 <- as.numeric(Road.Class == 3)
CONTROLDATA <- cbind(x1, x2, x3, x4, x5, x6, y)

x1t <- treatment[, 9]
x2t <- treatment[, 11]
x3t <- treatment[, 13] / 10000
classt <- treatment[, 19]
x4t <- as.numeric(classt == 1)
x5t <- as.numeric(classt == 2)
x6t <- as.numeric(classt == 3)
yt <- rowSums(treatment[, 2:4])
yt.after <- rowSums(treatment[, 5:7])
TREATMENTDATA <- cbind(x1t, x2t, x3t, x4t, x5t, x6t, yt)



#EB analysis
#Now use the APM from "nb.model" on Total.before
mu.EB<-vector("numeric",length(yt))
mu.EB<-nb.model(x1t,x2t,x3t,x4t,x5t,x6t)
#Compute the weights, alpha, and hence the EB estimates of casualty frequency
alpha.EB<-gamma/(gamma+mu.EB)
EB<-alpha.EB*mu.EB+(1-alpha.EB)*yt  
EB.sd<-sqrt(gamma+yt)/(1+gamma/mu.EB)

#Now the corresponding FB analysis
#n = number of MCMC iterations
#dataset.control = control (reference) dataset
#dataset.treatment = treated dataset
#beta*start = starting values for beta*
#rhostart = starting value for rho = log(kappa) = log(1/gamma) (see paper, pages 9-10)
#etastart = starting value for eta - the linear predictor 
#err* = MCMC random walk innovation variance for *
#sd* = prior standard deviation for *, where the prior in each case is N(0, (sd*)^2)


bayes<-function(n=10000, dataset.control,dataset.treatment,     beta0start=0,beta1start=0,beta2start=0,beta3start=0,beta4start=0,beta5start=0,beta6start=0,rhostart=0,     etastart=0,     errbeta0=0.3,errbeta1=0.3,errbeta2=0.3,errbeta3=0.3,errbeta4=0.3,errbeta5=0.3,errbeta6=0.3,            erreta=0.3, errrho=0.3,  sdbeta0=100,sdbeta1=100,sdbeta2=100,sdbeta3=100,sdbeta4=100,sdbeta5=100,sdbeta6=100,sdrho=100)
{
#Stores the control data
x1<-dataset.control[,1]     #Average speed
x2<-dataset.control[,2]     #% over the speed limit
x3<-dataset.control[,3]     #Traffic flow (/10000)
x4<-dataset.control[,4]     #Class of road: 1 if 1 (A)
x5<-dataset.control[,5]     #Class of road: 1 if 2 (B)
x6<-dataset.control[,6]     #Class of road: 1 if 3 (C)
y<-dataset.control[,7]      #Total number of casualties
k<-length(y)                #Number of sites in control

#Stores the treatment data
x1t<-dataset.treatment[,1]  #Average speed
x2t<-dataset.treatment[,2]  #% pver the speed limit
x3t<-dataset.treatment[,3]  #Traffic flow (/10000)
x4t<-dataset.treatment[,4]  #Class of road: 1 if 1 (A)
x5t<-dataset.treatment[,5]  #Class of road: 1 if 2 (B)
x6t<-dataset.treatment[,6]  #Class of road: 1 if 3 (C)
yt<-dataset.treatment[,7]   #Total number of casualties
kt<-length(yt)              #Number of treated sites

#Starting values, as provided by the user
beta0<-beta0start        
beta1<-beta1start     
beta2<-beta2start    #Regression parameters
beta3<-beta3start
beta4<-beta4start
beta5<-beta5start
beta6<-beta6start

rho<-rhostart        #log(kappa)
eta<-etastart        #mean(log(m))

#These vectors will store the candidate values for the parameters after the random walk update 
canbeta0<-vector("numeric",length=n)
canbeta1<-vector("numeric",length=n)
canbeta2<-vector("numeric",length=n)
canbeta3<-vector("numeric",length=n)
canbeta4<-vector("numeric",length=n)
canbeta5<-vector("numeric",length=n)
canbeta6<-vector("numeric",length=n)

canrho<-vector("numeric",length=n)
caneta<-matrix(ncol=length(x1t),nrow=n)   #Matrix, since we have a different "m" for each site

#Initialises the candidate vectors
canbeta0[1]<-beta0
canbeta1[1]<-beta1
canbeta2[1]<-beta2
canbeta3[1]<-beta3
canbeta4[1]<-beta4
canbeta5[1]<-beta5
canbeta6[1]<-beta6

canrho[1]<-rho
caneta[1,]<-eta    #Candidates for each eta are the same for every site

#These vectors will store the posterior draws
z0<-vector("numeric",length=n)
z1<-vector("numeric",length=n)
z2<-vector("numeric",length=n)
z3<-vector("numeric",length=n)
z4<-vector("numeric",length=n)
z5<-vector("numeric",length=n)
z6<-vector("numeric",length=n)

z7<-vector("numeric",length=n)

z8<-matrix(ncol=length(x1t),nrow=n)      #Matrix, one column for each treated site
mu<-matrix(ncol=length(x1t),nrow=n)      #Matrix, one column for each treated site
kappa<-vector("numeric",length=n)        #kappa=exp(rho)
gamma<-vector("numeric",length=n)        #gamma=1/kappa (as in the over-dispersion parameter estimated in "nlm.nb"

#Vectors of MCMC acceptance probabilities
aprobbeta0<-vector("numeric",length=n)
aprobbeta1<-vector("numeric",length=n)
aprobbeta2<-vector("numeric",length=n)
aprobbeta3<-vector("numeric",length=n)
aprobbeta4<-vector("numeric",length=n)
aprobbeta5<-vector("numeric",length=n)
aprobbeta6<-vector("numeric",length=n)

aprobrho<-vector("numeric",length=n)
aprobeta<-matrix(ncol=length(x1t),nrow=n) #Matrix, one column for each site

#Initialises the posterior draws at the starting values
z0[1]<-canbeta0[1]
z1[1]<-canbeta1[1]
z2[1]<-canbeta2[1]
z3[1]<-canbeta3[1]
z4[1]<-canbeta4[1]
z5[1]<-canbeta5[1]
z6[1]<-canbeta6[1]

z7[1]<-canrho[1]
z8[1,]<-caneta[1]

#Log-likelihood function for the negative binomial distribution
loglik.nb2<-function(k,X1,X2,X3,X4,X5,X6,Y,BETA0,BETA1,BETA2,BETA3,BETA4,BETA5,BETA6,RHO)
  {
    part1<-vector("numeric",length(Y))
    part2<-vector("numeric",length(Y))
    part3<-vector("numeric",length(Y))
    part4<-vector("numeric",length(Y))

    for(i in 1:length(Y))
      {
        part1[i]<-log(gamma(Y[i]+(1/exp(RHO))))

        part2[i]<-log(factorial(Y[i]))

        part3[i]<-Y[i]*(log(exp(RHO))+BETA0+BETA1*X1[i]+BETA2*X2[i]+BETA3*X3[i]+BETA4*X4[i]+BETA5*X5[i]+BETA6*X6[i])

        part4[i]<-(Y[i]+(1/exp(RHO)))*log(1+(exp(RHO))*exp(BETA0+BETA1*X1[i]+BETA2*X2[i]+BETA3*X3[i]+BETA4*X4[i]+BETA5*X5[i]+BETA6*X6[i]))
      }
    loglik<-sum(part1)-sum(part2)-(k*log(gamma(1/(exp(RHO)))))+sum(part3)-sum(part4)
    return(loglik)
  }


#Poisson log-likelihood function - used for the update of the eta's (log(m)'s)
loglik.poi<-function(k,dataset,ETA)
  {
    loglik<--(k*exp(ETA))+(k*mean(dataset)*ETA)-sum(log(factorial(dataset)))
    loglik
  }
        
#And now the MCMC iterations begin...
for(i in 2:n)
  {
    print(i)    #Prints the iteration number

    #Update for the constant term, beta_0, using a Normal prior with sd sdbeta0 (provided by the user)
    canbeta0[i]<-z0[i-1]+rnorm(1,0,errbeta0)
    likely0<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,canbeta0[i],z1[i-1],z2[i-1],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i-1],z1[i-1],z2[i-1],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta0[i]<-min(1,(likely0*((dnorm(canbeta0[i],0,sdbeta0)))/((dnorm(z0[i-1],0,sdbeta0)))))
    u<-runif(1)
    if(u<aprobbeta0[i]){z0[i]<-canbeta0[i]}
    if(u>=aprobbeta0[i]){z0[i]<-z0[i-1]}

   #Update for beta_1, using a Normal prior with sd sdbeta1 (provided by the user)
    canbeta1[i]<-z1[i-1]+rnorm(1,0,errbeta1)
    likely1<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],canbeta1[i],z2[i-1],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i-1],z2[i-1],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta1[i]<-min(1,(likely1*((dnorm(canbeta1[i],0,sdbeta1)))/((dnorm(z1[i-1],0,sdbeta1)))))
    u<-runif(1)
    if(u<aprobbeta1[i]){z1[i]<-canbeta1[i]}
    if(u>=aprobbeta1[i]){z1[i]<-z1[i-1]}

   #Update for beta_2, using a Normal prior with sd sdbeta2 (provided by the user)
    canbeta2[i]<-z2[i-1]+rnorm(1,0,errbeta2)
    likely2<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],canbeta2[i],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i-1],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta2[i]<-min(1,(likely2*((dnorm(canbeta2[i],0,sdbeta2)))/((dnorm(z2[i-1],0,sdbeta2)))))
    u<-runif(1)
    if(u<aprobbeta2[i]){z2[i]<-canbeta2[i]}
    if(u>=aprobbeta2[i]){z2[i]<-z2[i-1]}

   #Update for beta_3, using a Normal prior with sd sdbeta3 (provided by the user)
    canbeta3[i]<-z3[i-1]+rnorm(1,0,errbeta3)
    likely3<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],canbeta3[i],z4[i-1],z5[i-1],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i-1],z4[i-1],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta3[i]<-min(1,(likely3*((dnorm(canbeta3[i],0,sdbeta3)))/((dnorm(z3[i-1],0,sdbeta3)))))
    u<-runif(1)
    if(u<aprobbeta3[i]){z3[i]<-canbeta3[i]}
    if(u>=aprobbeta3[i]){z3[i]<-z3[i-1]}

   #Update for beta_4, using a Normal prior with sd sdbeta4 (provided by the user)
    canbeta4[i]<-z4[i-1]+rnorm(1,0,errbeta4)
    likely4<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],canbeta4[i],z5[i-1],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i-1],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta4[i]<-min(1,(likely4*((dnorm(canbeta4[i],0,sdbeta4)))/((dnorm(z4[i-1],0,sdbeta4)))))
    u<-runif(1)
    if(u<aprobbeta4[i]){z4[i]<-canbeta4[i]}
    if(u>=aprobbeta4[i]){z4[i]<-z4[i-1]}

   #Update for beta_5, using a Normal prior with sd sdbeta5 (provided by the user)
    canbeta5[i]<-z5[i-1]+rnorm(1,0,errbeta5)
    likely5<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],canbeta5[i],z6[i-1],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],z5[i-1],z6[i-1],z7[i-1])))
    aprobbeta5[i]<-min(1,(likely5*((dnorm(canbeta5[i],0,sdbeta5)))/((dnorm(z5[i-1],0,sdbeta5)))))
    u<-runif(1)
    if(u<aprobbeta5[i]){z5[i]<-canbeta5[i]}
    if(u>=aprobbeta5[i]){z5[i]<-z5[i-1]}

   #Update for beta_6, using a Normal prior with sd sdbeta6 (provided by the user)
    canbeta6[i]<-z6[i-1]+rnorm(1,0,errbeta6)
    likely6<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],z5[i],canbeta6[i],z7[i-1]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],z5[i],z6[i-1],z7[i-1])))
    aprobbeta6[i]<-min(1,(likely6*((dnorm(canbeta6[i],0,sdbeta6)))/((dnorm(z6[i-1],0,sdbeta6)))))
    u<-runif(1)
    if(u<aprobbeta6[i]){z6[i]<-canbeta6[i]}
    if(u>=aprobbeta6[i]){z6[i]<-z6[i-1]}

    #Update for rho=log(kappa), using a Normal prior with sd sdrho (provided by the user)
    canrho[i]<-z7[i-1]+rnorm(1,0,errrho)
    likely7<-exp((loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],z5[i],z6[i],canrho[i]))-(loglik.nb2(k,x1,x2,x3,x4,x5,x6,y,z0[i],z1[i],z2[i],z3[i],z4[i],z5[i],z6[i],z7[i-1])))
    aprobrho[i]<-min(1,(likely7*((dnorm(canrho[i],0,sdrho)))/((dnorm(z7[i-1],0,sdrho)))))  
    u<-runif(1)
    if(u<aprobrho[i]){z7[i]<-canrho[i]}
    if(u>=aprobrho[i]){z7[i]<-z7[i-1]}

    #Now we find mu=exp(beta0+beta1*x1+beta2*x2+beta3*x3+beta4*x4) for each treatment site
    for(j in 1:length(x1t))
        {
          mu[i,j]<-exp(z0[i]+(z1[i]*x1t[j])+(z2[i]*x2t[j])+(z3[i]*x3t[j])+(z4[i]*x4t[j])+(z5[j]*x5t[j])+(z6[j]*x6t[j]))
        }

    #Now we calculate kappa=exp(rho)...
    kappa[i]<-exp(z7[i])

    #...and now gamma=1/kappa
    gamma[i]<-(1/kappa[i])
 
    #Candidate values for eta[i], followed by random walk update for eta (=log(m)) for each site.  In line with truly EB approach, we use a gamma prior for m (conjugate for the Poisson), and so we adjust the prior for eta by the Jacobian determinant (exp(eta))
    likely8<-vector("numeric",length(x1t))
    for(j in 1:length(likely8))
      {
        caneta[i,j]<-z8[i-1,j]+rnorm(1,0,erreta[j])
        likely8[j]<-exp((loglik.poi(1,yt[j],caneta[i,j]))-(loglik.poi(1,yt[j],z8[i-1,j]))) #Likelihood

        numerator<-((dgamma(exp(caneta[i,j]),shape=gamma[i],rate=(gamma[i]/mu[i,j]))*exp(caneta[i,j])))
        denominator<-((dgamma(exp(z8[i-1,j]),shape=gamma[i],rate=(gamma[i]/mu[i,j]))*exp(z8[i-1,j])))
        if(denominator>0){
          aprobeta[i,j]<-min(1,(likely8[j]*numerator/denominator))
        }
        else(aprobeta[i,j]<-min(1,(likely8[j]*numerator/(denominator+0.0000000000000001))))

    u<-runif(1)
    if(u<aprobeta[i,j]){z8[i,j]<-caneta[i,j]}
    if(u>=aprobeta[i,j]){z8[i,j]<-z8[i-1,j]}
      }
  }

#Vectors of acceptance probabilities without NA's (first values)
aprobbeta0<-mean(aprobbeta0[!is.na(aprobbeta0)])
aprobbeta1<-mean(aprobbeta1[!is.na(aprobbeta1)])
aprobbeta2<-mean(aprobbeta2[!is.na(aprobbeta2)])
aprobbeta3<-mean(aprobbeta3[!is.na(aprobbeta3)])
aprobbeta4<-mean(aprobbeta4[!is.na(aprobbeta4)])
aprobbeta5<-mean(aprobbeta5[!is.na(aprobbeta5)])
aprobbeta6<-mean(aprobbeta6[!is.na(aprobbeta6)])

aprobrho<-mean(aprobrho[!is.na(aprobrho)])
aprobeta2<-matrix(ncol=length(x1t),nrow=(n-1))

for(j in 1:length(x1t))
  {
    temp<-aprobeta[,j]
    aprobeta2[,j]<-temp[!is.na(temp)]
  }

aprobeta<-vector("numeric",length(x1t))
for(i in 1:length(aprobeta))
  {
    aprobeta[i]<-mean(aprobeta2[,i])
  }

#Vectors of posterior draws, more appropriately labelled
beta0<-z0
beta1<-z1
beta2<-z2
beta3<-z3
beta4<-z4
beta5<-z5
beta6<-z6

rho<-z7
kappa<-exp(rho)
eta<-z8
FB<-exp(eta)
T<-vector("numeric",length(n))
for(i in 1:n)
  {
    T[i]<-sum(FB[i,])
  }

results<-list("beta0"=beta0,"beta1"=beta1,"beta2"=beta2,"beta3"=beta3,"beta4"=beta4,"beta5"=beta5,"beta6"=beta6,"rho"=rho,"kappa"=kappa,"gamma"=gamma,"mu"=mu,"eta"=eta,"FB"=FB,"T"=T)
#Return values
return(results)
}

        
#Different innovation standard deviations, after much trial and improvement, for eta_j
ERRETA<-c(1.25,2.05,1.75,2.30,1.30,3.00,2.00,1.75,1.75,1.75,1.55,1.75,1.35,2.00,2.10,1.55,1.75,1.55,1.20,2.50,2.10,2.60,1.45,2.05,1.65,1.55,1.75,1.75,1.25,1.25,2.05,1.75,1.05,1.25,2.20,1.25,2.20,2.25,2.00,1.75,1.75,1.75,1.75,1.80,2.50,1.55,1.05,1.75,1.50,1.80,1.50,2.25,1.75,1.35,1.50,1.25)

#Now run the MCMC!

iterations<-10000
B=1000  #burn-in

start.beta0<-10
start.beta1<-10
start.beta2<--0.013
start.beta3<-0.444
start.beta4<-0.674
start.beta5<-0.845
start.beta6<-1.060

#start.beta0<-5
#start.beta1<-5
#start.beta2<-5
#start.beta3<-5
#start.beta4<-5
#start.beta5<-5
#start.beta6<-5

#Don't worry about this one!
start.eta<-log(20)
start.rho<-log(0.4)

prior.sd.beta0<-100
prior.sd.beta1<-100
prior.sd.beta2<-100
prior.sd.beta3<-100
prior.sd.beta4<-100
prior.sd.beta5<-100
prior.sd.beta6<-100


prior.sd.rho<-1000

test<-bayes(iterations,CONTROLDATA,TREATMENTDATA,start.beta0,start.beta1,start.beta2,start.beta3,start.beta4,start.beta5,start.beta6,start.rho,start.eta,0.6,0.018,0.013,0.5,0.6,0.6,0.6,ERRETA,0.8,prior.sd.beta0,prior.sd.beta1,prior.sd.beta2,prior.sd.beta3,prior.sd.beta4,prior.sd.beta5,prior.sd.beta6,prior.sd.rho)

#Then, for example:
#plot(ts(test$beta0))
#Plots the MCMC trace for beta0

#For comparison with EB:

#mu, alpha, mean, sd:
#mu.FB =vector("numeric", 56)
#FB.mean =vector("numeric", 56)
#FB.median =vector("numeric", 56)
#FB.sd = vector("numeric", 56)
#for(i in 1:56){
#mu.FB[i] = mean(test$mu[(B+1):iterations, i])
#FB.mean[i] = mean(test$FB[(B+1):iterations, i])
#FB.median[i] = median(test$FB[(B+1):iterations, i])
#FB.sd[i] = sd(test$FB[(B+1):iterations, i])}

#par(mfrow=c(1,3))
#plot(mu.EB, mu.FB)
#abline(0,1)
#plot(EB, FB)
#abline(0,1)
#plot(EB.sd, FB.sd)
#abline(0,1)

#Total?
#total.EB = sum(ceiling(EB))
#total.FB = sum(ceiling(FB.mean))
#total.FBmedian = sum(ceiling(FB.median))



