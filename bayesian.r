library(MASS)
#Read in the control sites data, and then select the variables used
control <- read.table("C:/Users/sharv/Downloads/control.txt", header = TRUE)
x1 <- Av.Sp
x2 <- X.ov.lim
x3 <- Flow / 10000
y <- Total
x4 <- as.numeric(Road.Class == 1)
x5 <- as.numeric(Road.Class == 2)
x6 <- as.numeric(Road.Class == 3)
CONTROLDATA <- cbind(x1, x2, x3, x4, x5, x6, y)
treatment <- read.table("C:/Users/sharv/Downloads/treated.txt", header = TRUE)

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

bayes <- function(n = 10000, dataset.control, dataset.treatment, beta0start = 0, beta1start = 0, beta2start = 0, beta3start = 0, beta4start = 0, beta5start = 0, beta6start = 0, rhostart = 0, etastart = 0,  errbeta0=0.3,errbeta1=0.3,errbeta2=0.3,errbeta3=0.3,errbeta4=0.3,errbeta5=0.3,errbeta6=0.3, erreta = 0.3, errrho = 0.3,sdbeta0=100,sdbeta1=100,sdbeta2=100,sdbeta3=100,sdbeta4=100,sdbeta5=100,sdbeta6=100, sdrho = 100) {
  control <- as.matrix(dataset.control)
  treatment <- as.matrix(dataset.treatment)
  y <- control[, 7]
  yt <- treatment[, 7]
  k <- length(y)
  kt <- length(yt)
  beta_start <- c(beta0start, beta1start, beta2start, beta3start, beta4start, beta5start, beta6start)
  errbeta<-c(errbeta0,errbeta1,errbeta2,errbeta3,errbeta4,errbeta5,errbeta6)
  sdbeta<-c(sdbeta0,sdbeta1,sdbeta2,sdbeta3,sdbeta4,sdbeta5,sdbeta6)
  rho <- rhostart
  eta <- rep(etastart, length(treatment[, 1]))
  canbeta <- matrix(0, n, 7)
  z <- matrix(0, n, 7)
  canbeta[1, ] <- beta_start
  z[1, ] <- beta_start
  canrho <- rep(0, n)
  caneta <- matrix(0, n, length(treatment[, 1]))
  canrho[1] <- rho
  caneta[1, ] <- eta
  z7 <- rep(0, n)
  z8 <- matrix(0, n, length(treatment[, 1]))
  z7[1] <- canrho[1]
  z8[1, ] <- caneta[1, ]
  mu <- matrix(0, n, length(treatment[, 1]))
  kappa <- rep(0, n)
  gamma <- rep(0, n)
  aprobbeta <- matrix(0, n, 7)
  aprobrho <- rep(0, n)
  aprobeta <- matrix(0, n, length(treatment[, 1]))
  
  # Log-likelihood function for the negative binomial distribution
  loglik.nb2 <- function(k, X, Y, beta, rho) {
    B <- cbind(1, X) %*% beta
    part1 <- lgamma(Y + 1/exp(rho))
    part2 <- lgamma(1/exp(rho))
    part3 <- Y * (log(exp(rho)) + B)
    part4 <- (Y + 1/exp(rho)) * log(1 + exp(rho) * exp(B))
    sum(part1) - sum(lgamma(Y + 1)) - k * part2 + sum(part3) - sum(part4)
  }
  
  # Poisson log-likelihood function - used for the update of the eta's (log(m)'s)
  loglik.poi <- function(k, dataset, ETA) {
    -(k * exp(ETA)) + (k * mean(dataset) * ETA) - sum(lgamma(dataset + 1))
  }
  
  # MCMC iterations
  for (i in 2:n) {
    print(i)
    
    for (j in 1:7) {
      canbeta[i, j] <- z[i - 1, j] + rnorm(1, 0, errbeta[j])
      likely <- exp(loglik.nb2(k, control[, -7], y, canbeta[i, ], z7[i - 1]) - loglik.nb2(k, control[, -7], y, z[i - 1, ], z7[i - 1]))
      aprobbeta[i, j] <- min(1, (likely * dnorm(canbeta[i, j], 0, sdbeta[j])) / dnorm(z[i - 1, j], 0, sdbeta[j]))
      if (runif(1) < aprobbeta[i, j]) {
        z[i, j] <- canbeta[i, j]
      } else {
        z[i, j] <- z[i - 1, j]
      }
    }
    
    canrho[i] <- z7[i - 1] + rnorm(1, 0, errrho)
    likely <- exp(loglik.nb2(k, control[, -7], y, z[i, ], canrho[i]) - loglik.nb2(k, control[, -7], y, z[i, ], z7[i - 1]))
    aprobrho[i] <- min(1, (likely * dnorm(canrho[i], 0, sdrho)) / dnorm(z7[i - 1], 0, sdrho))
    if (runif(1) < aprobrho[i]) {
      z7[i] <- canrho[i]
    } else {
      z7[i] <- z7[i - 1]
    }
    
    for (j in 1:length(treatment[, 1])) {
      mu[i, j] <- exp(z[i, ] %*% c(1, treatment[j, -7]))
    }
    
    kappa[i] <- exp(z7[i])
    gamma[i] <- 1 / kappa[i]
    
    for (j in 1:length(treatment[, 1])) {
      caneta[i, j] <- z8[i - 1, j] + rnorm(1, 0, erreta)
      likely <- exp(loglik.poi(1, yt[j], caneta[i, j]) - loglik.poi(1, yt[j], z8[i - 1, j]))
      numerator <- dgamma(exp(caneta[i, j]), shape = gamma[i], rate = gamma[i] / mu[i, j]) * exp(caneta[i, j])
      denominator <- dgamma(exp(z8[i - 1, j]), shape = gamma[i], rate = gamma[i] / mu[i, j]) * exp(z8[i - 1, j])
      aprobeta[i, j] <- if (denominator > 0) min(1, (likely * numerator) / denominator) else min(1, (likely * numerator) / (denominator + 1e-16))
      if (runif(1) < aprobeta[i, j]) {
        z8[i, j] <- caneta[i, j]
      } else {
        z8[i, j] <- z8[i - 1, j]
      }
    }
  }
  
  # Acceptance probabilities
  aprobbeta_means <- colMeans(aprobbeta, na.rm = TRUE)
  aprobrho_mean <- mean(aprobrho, na.rm = TRUE)
  aprobeta_means <- colMeans(aprobeta, na.rm = TRUE)
  
  # Vectors of posterior draws, more appropriately labelled
  beta <- z
  eta <- z8
  FB <- exp(eta)
  T <- rowSums(FB)
  
  results <- list(beta0 = beta[, 1], beta1 = beta[, 2], beta2 = beta[, 3], beta3 = beta[, 4], beta4 = beta[, 5], beta5 = beta[, 6], beta6 = beta[, 7], rho = z7, kappa = kappa, gamma = gamma, mu = mu, eta = eta, FB = FB, T = T)
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


bayes_lognormal <- function(n = 10000, dataset.control, dataset.treatment, 
                  beta0start = 0, beta1start = 0, beta2start = 0, beta3start = 0, beta4start = 0, beta5start = 0, beta6start = 0, 
                  rhostart = 0, etastart = 0,  
                  errbeta0 = 0.3, errbeta1 = 0.3, errbeta2 = 0.3, errbeta3 = 0.3, errbeta4 = 0.3, errbeta5 = 0.3, errbeta6 = 0.3, 
                  erreta = 0.3, errrho = 0.3,
                  sdbeta0 = 100, sdbeta1 = 100, sdbeta2 = 100, sdbeta3 = 100, sdbeta4 = 100, sdbeta5 = 100, sdbeta6 = 100, 
                  sdrho = 100) {
  
  control <- as.matrix(dataset.control)
  treatment <- as.matrix(dataset.treatment)
  
  y <- control[, 7]
  yt <- treatment[, 7]
  
  k <- length(y)
  kt <- length(yt)
  
  beta_start <- c(beta0start, beta1start, beta2start, beta3start, beta4start, beta5start, beta6start)
  errbeta <- c(errbeta0, errbeta1, errbeta2, errbeta3, errbeta4, errbeta5, errbeta6)
  sdbeta <- c(sdbeta0, sdbeta1, sdbeta2, sdbeta3, sdbeta4, sdbeta5, sdbeta6)
  
  rho <- rhostart
  eta <- rep(etastart, length(treatment[, 1]))
  
  canbeta <- matrix(0, n, 7)
  z <- matrix(0, n, 7)
  canbeta[1, ] <- beta_start
  z[1, ] <- beta_start
  
  canrho <- rep(0, n)
  caneta <- matrix(0, n, length(treatment[, 1]))
  canrho[1] <- rho
  caneta[1, ] <- eta
  
  z7 <- rep(0, n)
  z8 <- matrix(0, n, length(treatment[, 1]))
  z7[1] <- canrho[1]
  z8[1, ] <- caneta[1, ]
  
  mu <- matrix(0, n, length(treatment[, 1]))
  kappa <- rep(0, n)
  gamma <- rep(0, n)
  
  aprobbeta <- matrix(0, n, 7)
  aprobrho <- rep(0, n)
  aprobeta <- matrix(0, n, length(treatment[, 1]))
  
  # Log-likelihood function for the negative binomial distribution
  loglik.nb2 <- function(k, X, Y, beta, rho) {
    B <- cbind(1, X) %*% beta
    part1 <- lgamma(Y + 1 / exp(rho))
    part2 <- lgamma(1 / exp(rho))
    part3 <- Y * (log(exp(rho)) + B)
    part4 <- (Y + 1 / exp(rho)) * log(1 + exp(rho) * exp(B))
    sum(part1) - sum(lgamma(Y + 1)) - k * part2 + sum(part3) - sum(part4)
  }
  
  # Poisson log-likelihood function - used for the update of the eta's (log(m)'s)
  loglik.poi <- function(k, dataset, ETA) {
    -(k * exp(ETA)) + (k * mean(dataset) * ETA) - sum(lgamma(dataset + 1))
  }
  
  # MCMC iterations
  for (i in 2:n) {
    print(i)
    
    for (j in 1:7) {
      canbeta[i, j] <- z[i - 1, j] + rnorm(1, 0, errbeta[j])
      likely <- exp(loglik.nb2(k, control[, -7], y, canbeta[i, ], z7[i - 1]) - loglik.nb2(k, control[, -7], y, z[i - 1, ], z7[i - 1]))
      aprobbeta[i, j] <- min(1, (likely * dnorm(canbeta[i, j], 0, sdbeta[j])) / dnorm(z[i - 1, j], 0, sdbeta[j]))
      if (runif(1) < aprobbeta[i, j]) {
        z[i, j] <- canbeta[i, j]
      } else {
        z[i, j] <- z[i - 1, j]
      }
    }
    
    canrho[i] <- z7[i - 1] + rnorm(1, 0, errrho)
    likely <- exp(loglik.nb2(k, control[, -7], y, z[i, ], canrho[i]) - loglik.nb2(k, control[, -7], y, z[i, ], z7[i - 1]))
    aprobrho[i] <- min(1, (likely * dnorm(canrho[i], 0, sdrho)) / dnorm(z7[i - 1], 0, sdrho))
    if (runif(1) < aprobrho[i]) {
      z7[i] <- canrho[i]
    } else {
      z7[i] <- z7[i - 1]
    }
    
    for (j in 1:length(treatment[, 1])) {
      mu[i, j] <- exp(z[i, ] %*% c(1, treatment[j, -7]))
    }
    
    kappa[i] <- exp(z7[i])
    gamma[i] <- 1 / kappa[i]
    
    for (j in 1:length(treatment[, 1])) {
      caneta[i, j] <- z8[i - 1, j] + rnorm(1, 0, erreta)
      likely <- exp(loglik.poi(1, yt[j], caneta[i, j]) - loglik.poi(1, yt[j], z8[i - 1, j]))
      numerator <- dlnorm(exp(caneta[i, j]), meanlog = log(mu[i, j]), sdlog = sqrt(gamma[i])) * exp(caneta[i, j])
      denominator <- dlnorm(exp(z8[i - 1, j]), meanlog = log(mu[i, j]), sdlog = sqrt(gamma[i])) * exp(z8[i - 1, j])
      aprobeta[i, j] <- if (denominator > 0) min(1, (likely * numerator) / denominator) else min(1, (likely * numerator) / (denominator + 1e-16))
      if (runif(1) < aprobeta[i, j]) {
        z8[i, j] <- caneta[i, j]
      } else {
        z8[i, j] <- z8[i - 1, j]
      }
    }
  }
  
  # Acceptance probabilities
  aprobbeta_means <- colMeans(aprobbeta, na.rm = TRUE)
  aprobrho_mean <- mean(aprobrho, na.rm = TRUE)
  aprobeta_means <- colMeans(aprobeta, na.rm = TRUE)
  
  # Vectors of posterior draws, more appropriately labelled
  beta <- z
  eta <- z8
  FB <- exp(eta)
  T <- rowSums(FB)
  
  results <- list(beta0 = beta[, 1], beta1 = beta[, 2], beta2 = beta[, 3], beta3 = beta[, 4], beta4 = beta[, 5], beta5 = beta[, 6], beta6 = beta[, 7], rho = z7, kappa = kappa, gamma = gamma, mu = mu, eta = eta, FB = FB, T = T)
  return(results)
}

test_lognormal<-bayes_lognormal(iterations,CONTROLDATA,TREATMENTDATA,start.beta0,start.beta1,start.beta2,start.beta3,start.beta4,start.beta5,start.beta6,start.rho,start.eta,0.6,0.018,0.013,0.5,0.6,0.6,0.6,ERRETA,0.8,prior.sd.beta0,prior.sd.beta1,prior.sd.beta2,prior.sd.beta3,prior.sd.beta4,prior.sd.beta5,prior.sd.beta6,prior.sd.rho)



#test_weibull<-bayes_weibull(iterations,CONTROLDATA,TREATMENTDATA,start.beta0,start.beta1,start.beta2,start.beta3,start.beta4,start.beta5,start.beta6,start.rho,start.eta,0.6,0.018,0.013,0.5,0.6,0.6,0.6,ERRETA,0.8,prior.sd.beta0,prior.sd.beta1,prior.sd.beta2,prior.sd.beta3,prior.sd.beta4,prior.sd.beta5,prior.sd.beta6,prior.sd.rho)


