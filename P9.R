library(rlang)
library(ggplot2)
library(distr6)
library(purrr)
library(latex2exp)
library(tidyverse)
library(lubridate)
library(purrr)
library(stats)


Qn <- function(x,yt){
  n <- length(yt)
  sum <- 0
  for(i in 2:n){
    if(yt[i-1]<=x){
      sum <- sum+(yt[i]-yt[i-1])
    }
  }
  return((1/sqrt(n))*sum)
}
library(purrr)

absQn <- function(x,yt){
  n <- length(yt)
  sum <- 0
  for(i in 2:n){
    if(yt[i-1]<=x){
      sum <- sum+(yt[i]-yt[i-1])
    }
  }
  return(abs((1/sqrt(n))*sum))
}

Sn <- function(data){
  Q_obs = partial(absQn, yt = data)
  map(data, Q_obs) %>% unlist() %>% max
}
#Sn(data)

Tn <- function(yt){
  sum <- 0
  n <- length(yt)
  for(i in 2:length(yt)){
    sum <- sum+Qn(yt[i-1],yt)^2
  }
  return((1/n)*sum)
}
#Tn(data)

#Calculate the distribution of the test statistics.
#Funciton to simulate data
Simulate_data <- function(n_points){
  y <- numeric(n_points)  # Vector to store simulated data
  y[1] <- 0  # Set the initial value
  for (t in 2:n_points) {
    # Simulate y_t based on the martingale property
    y[t] <- rnorm(1) + y[t-1]  # You can use a random walk as an example
  }
  return(y)
}

MonteCarlo <- function(n_simulations,n_points){
  test_statistic_values_Sn <- numeric(n_simulations)
  test_statistic_values_Tn <- numeric(n_simulations)
  percent <- 0
  for (i in 1:n_simulations) {
    if((i/n_simulations)*100>percent){
      print(percent)
      percent <- percent +5
    }
    # Simulate data under the null hypothesis
    simulated_data <- Simulate_data(n_points)
    # Calculate the test statistic for the simulated data
    test_statistic_values_Sn[i] <- Sn(simulated_data)
    test_statistic_values_Tn[i] <- Tn(simulated_data)
  }
  return(list("Sn"=test_statistic_values_Sn,"Tn"=test_statistic_values_Tn))
}
#results<- MonteCarlo(10000,10000)

#standarisere.
setwd("/Users/sorenthode/Dropbox/Mac/Desktop/P9")
Tn_data <- read.csv("TableSnandTn10000x10000.csv")
Sn_data <- read.csv("TableSnandTn10000x1000Sn2.csv")

quantile(Tn_data$Tn,probs=c(0.01,0.05,0.1,0.9,0.95,0.99))
quantile(Sn_data$Sn,probs=c(0.01,0.05,0.1,0.9,0.95,0.99))
plot(density(Sn_data$Sn),xlim=c(0,5))
plot(density(Tn_data$Tn),xlim=c(0,5))

data.frame(data2)

psn <- ggplot(Sn_data, aes(x=Sn)) + 
  geom_density(fill="gray")+
  labs(x = unname(TeX("$S_n$")), y = "Density")+
  theme_classic() +
  xlim(0,5) +
  geom_density()
psn

ptn <- ggplot(Tn_data, aes(x=Tn)) + 
  geom_density(fill="gray")+
  labs(x = unname(TeX("$T_n$")), y = "Density")+
  theme_classic() +
  xlim(-0.5,5) +
  geom_density()
ptn

#Data part. Download data.
library(DBI)
library(RSQLite)
setwd("/Users/sorenthode/Dropbox/Mac (2)/Desktop")
library(DBI)
library(RSQLite)
library(dplyr)

#Connect to db
actuals_connection <- dbConnect(drv=RSQLite::SQLite(), dbname="actuals.db")
forecast_connection <- dbConnect(drv=RSQLite::SQLite(), dbname="forecasts.db")

actuals <- dbGetQuery(conn=actuals_connection, statement=paste("SELECT * FROM Actuals"))
forecasts <- dbGetQuery(conn=forecast_connection, statement=paste("SELECT * FROM Forecasts"))
names(forecasts)[3] <- "Forecasts"
df <- merge(x=actuals,y=forecasts,by="Start")
df <- na.omit(df)

quantileS_0.99 <- quantile(Sn_data$Sn,0.01)
quantileS_0.95 <- quantile(Sn_data$Sn,0.05)
quantileS_0.90 <- quantile(Sn_data$Sn,0.10)

quantileT_0.99 <- quantile(Tn_data$Tn,0.01)
quantileT_0.95 <- quantile(Tn_data$Tn,0.05)
quantileT_0.90 <- quantile(Tn_data$Tn,0.10)
returnlist <- list()
uniques_start <- unique(df$Start)
starttimelist <- list()
j <- 1
for(i in uniques_start){
  starttimelist[[j]] <- df2[df2$Start ==i,]
  j <- j+1
}
for(i in 1:length(uniques_start)){
  x <- starttimelist[[i]]
  x <- arrange(x,SamplingTime)
  #x$Forecasts - x$Value
  xdiff <- diff(x$Forecasts)
  sigma <- sd(xdiff)
  mu = mean(xdiff)
  n = length(xdiff)
  if(n>=1){
    #t-test
    #(xdiff-mu)/(sigma/sqrt(n))
    H0_0.99 <- t.test(xdiff,conf.level = 0.99)$p.value < 0.01
    H0_0.95 <- t.test(xdiff,conf.level = 0.95)$p.value < 0.05
    H0_0.90 <- t.test(xdiff,conf.level = 0.90)$p.value < 0.10
    H0 <- mu-1.96*(sigma/sqrt(n)) <= 0 & 0 <= mu+1.96*(sigma/sqrt(n))

        #Standardiser
    diffz=xdiff/sigma
    z=cumsum(diffz)
    
    #Regn teststørrelser
    S <- Sn(z)
    T_ <- Tn(z)

    H1_0.99 <- as.numeric( 0 <= S & S <= quantileS_0.99)
    H1_0.95 <- as.numeric( 0 <= S & S <= quantileS_0.95)
    H1_0.90 <- as.numeric( 0 <= S & S <= quantileS_0.90)
    
    H2_0.99 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.99)
    H2_0.95 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.95)
    H2_0.90 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.90)
  }else{
    H0 <- NA
    S <- NA
    T_ <- NA
    
    H0_0.99 <- NA
    H0_0.95 <- NA
    H0_0.90 <- NA
    
    H1_0.99 <- NA
    H1_0.95 <- NA
    H1_0.90 <- NA
    
    H2_0.99 <- NA
    H2_0.95 <- NA
    H2_0.90 <- NA
  }
  #The diff between forecasts and value
  actual_diff <- mean(x$Forecasts)-mean(x$Value)
  returnlist[[i]] <- c(mu,sigma,H0,n,S,T_,H1_0.99,H1_0.95,H1_0.90,H2_0.99,H2_0.95,H2_0.90,actual_diff,H0_0.99,H0_0.95,H0_0.90,x[1,1])
}
returnlist

#The first entry is na.
returnlist_nona <- returnlist[-1]

#Tjekker hvor mange der har bestået testen
H0_0.99_sum <- sum(na.omit(as.vector(unlist(lapply(returnlist_nona, `[[`, 14)))))
H0_0.95_sum <- sum(na.omit(as.vector(unlist(lapply(returnlist_nona, `[[`, 15)))))
H0_0.90_sum <- sum(na.omit(as.vector(unlist(lapply(returnlist_nona, `[[`, 16)))))
H1_0.99_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 7))))
H1_0.95_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 8))))
H1_0.90_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 9))))
H2_0.99_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 10))))
H2_0.95_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 11))))
H2_0.90_sum <- sum(as.vector(unlist(lapply(returnlist_nona, `[[`, 12))))
#Find % af dem
n <- length(as.vector(unlist(lapply(returnlist_nona, `[[`, 6))))
H0_0.99_per <- H0_0.99_sum/n
H0_0.95_per <- H0_0.95_sum/n
H0_0.90_per <- H0_0.90_sum/n
H1_0.99_per <- H1_0.99_sum/n
H1_0.95_per <- H1_0.95_sum/n
H1_0.90_per <- H1_0.90_sum/n
H2_0.99_per <- H2_0.99_sum/n
H2_0.95_per <- H2_0.95_sum/n
H2_0.90_per <- H2_0.90_sum/n

#Plot
S_data <- as.vector(unlist(lapply(returnlist_nona, `[[`, 5)))
T_data <- as.vector(unlist(lapply(returnlist_nona, `[[`, 6)))
df_S_data_and_sim <- data.frame(dataS =c(S_data,Sn_data$Sn), group=rep(c('Data','Sim'),times=c(length(S_data),length(Sn_data$Sn))))
df_T_data_and_sim <- data.frame(dataT =c(T_data,Tn_data$Tn), group=rep(c('Data','Sim'),times=c(length(T_data),length(Tn_data$Tn))))

psnS_data <- ggplot(df_S_data_and_sim, aes(x=dataS, color=group)) + 
  labs(x = unname(TeX("$S$")), y = "Density")+
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.8, 0.6)) +
  geom_density() 
psnS_data

psnT_data <- ggplot(df_T_data_and_sim, aes(x=dataT, color=group)) + 
  labs(x = unname(TeX("$T$")), y = "Density")+
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.8, 0.6)) +
  geom_density() 
psnT_data

#Test om forskellen på forcasts og actuals er cray
mean_diff <- sum(df$Forecasts-df$Value)
mean_diff_per <- mean_diff/length(df$Value)
nrow(df)
returnlistfor <- c()
returnlistac <- c()
l <- 1
uniques <- unique(df$Start)
for(i in uniques){
  x <- df[df$Start ==i,]
  returnlistfor[l] <- mean(x$Forecasts)
  returnlistac[l] <- mean(x$Value)
  l <- l+1
}
dfscatter <- data_frame(returnlistfor,returnlistac)
psnscatter1 <- ggplot(dfscatter, aes(x=returnlistfor,y=returnlistac)) + 
  labs(x = unname(TeX("Forecasts")), y = "Actuals")+
  theme_classic() +
  geom_point() +
  xlim(0,11000) +
  ylim(0,15000) +
  geom_abline(intercept = 0,slope=1,color="blue")
psnscatter1

df2 <- df
df2$Start <- as.POSIXct(df2$Start,format="%Y-%m-%dT%H:%M:%S")
df2$SamplingTime <- as.POSIXct(df2$SamplingTime,format="%Y-%m-%dT%H:%M:%S")

TimeDifference <- c()
for(i in 1:length(df2$Start)){
  TimeDifference[i] <- as.numeric(df2$Start[i]) - as.numeric(df2$SamplingTime[i])
}
df2$TimeDifference <- TimeDifference
df2[df2$TimeDifference < 0,] <- NA

df2 <- na.omit(df2)
df2 <- df2[order(df2$TimeDifference),]

quantileS_0.99 <- quantile(Sn_data$Sn,0.01)
quantileS_0.95 <- quantile(Sn_data$Sn,0.05)
quantileS_0.90 <- quantile(Sn_data$Sn,0.10)

quantileT_0.99 <- quantile(Tn_data$Tn,0.01)
quantileT_0.95 <- quantile(Tn_data$Tn,0.05)
quantileT_0.90 <- quantile(Tn_data$Tn,0.10)


returnlistdiff <- list()
uniques_timediff <- unique(df2$TimeDifference)
timedifflist <- list()
j <- 1
for(i in uniques_timediff){
  timedifflist[[j]] <- df2[df2$TimeDifference ==i,]
  j <- j+1
}
#timedifflistsafe <- timedifflist
for(i in 1:length(uniques_timediff)){
  x <- timedifflist[[i]]
  x <- arrange(x,SamplingTime)
  #x$Forecasts - x$Value
  xdiff <- diff(x$Forecasts)
  sigma <- sd(xdiff)
  mu = mean(xdiff)
  n = length(xdiff)
  if(n>=10){
    #t-test
    #(xdiff-mu)/(sigma/sqrt(n))
    
    H0_0.99 <- t.test(xdiff,conf.level = 0.99)$p.value < 0.01
    H0_0.95 <- t.test(xdiff,conf.level = 0.99)$p.value < 0.05
    H0_0.90 <- t.test(xdiff,conf.level = 0.99)$p.value < 0.10
    
    H0 <- mu-1.96*(sigma/sqrt(n)) <= 0 & 0 <= mu+1.96*(sigma/sqrt(n))
    
    #Standardiser
    diffz=xdiff/sigma
    z=cumsum(diffz)
    
    #Regn teststørrelser
    S <- Sn(z)
    T_ <- Tn(z)
    
    H1_0.99 <- as.numeric( 0 <= S & S <= quantileS_0.99)
    H1_0.95 <- as.numeric( 0 <= S & S <= quantileS_0.95)
    H1_0.90 <- as.numeric( 0 <= S & S <= quantileS_0.90)
    
    H2_0.99 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.99)
    H2_0.95 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.95)
    H2_0.90 <- as.numeric( 0 <= T_ & T_ <= quantileT_0.90)
  }else{
    H0 <- NA
    S <- NA
    T_ <- NA
    
    H0_0.99 <- NA
    H0_0.95 <- NA
    H0_0.90 <- NA
    
    H1_0.99 <- NA
    H1_0.95 <- NA
    H1_0.90 <- NA
    
    H2_0.99 <- NA
    H2_0.95 <- NA
    H2_0.90 <- NA
  }
  returnlistdiff[[i]] <- c(mu,sigma,H0,n,S,T_,H1_0.99,H1_0.95,H1_0.90,H2_0.99,H2_0.95,H2_0.90,i,H0_0.99,H0_0.95,H0_0.90)
}
returnlistdiff
write.csv(returnlistdiff,"/Users/sorenthode/Dropbox/Mac (2)/Desktop/returnlistdiff2")

#How many passed the test?
H0_0.99_sum_2 <- sum(na.omit(as.vector(unlist(lapply(returnlistdiff, `[[`, 14)))))
H0_0.95_sum_2 <- sum(na.omit(as.vector(unlist(lapply(returnlistdiff, `[[`, 15)))))
H0_0.90_sum_2 <- sum(na.omit(as.vector(unlist(lapply(returnlistdiff, `[[`, 16)))))
H1_0.99_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 7))))
H1_0.95_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 8))))
H1_0.90_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 9))))
H2_0.99_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 10))))
H2_0.95_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 11))))
H2_0.90_sum_2 <- sum(as.vector(unlist(lapply(returnlistdiff, `[[`, 12))))
#Find % af dem
n_2 <- length(as.vector(unlist(lapply(returnlistdiff, `[[`, 6))))
H0_0.99_per_2 <- H0_0.99_sum/n
H0_0.95_per_2 <- H0_0.95_sum/n
H0_0.90_per_2 <- H0_0.90_sum/n
H1_0.99_per_2 <- H1_0.99_sum/n
H1_0.95_per_2 <- H1_0.95_sum/n
H1_0.90_per_2 <- H1_0.90_sum/n
H2_0.99_per_2 <- H2_0.99_sum/n
H2_0.95_per_2 <- H2_0.95_sum/n
H2_0.90_per_2 <- H2_0.90_sum/n


#Plots igen
S_data_diff <- as.vector(unlist(lapply(returnlistdiff, `[[`, 5)))
T_data_diff <- as.vector(unlist(lapply(returnlistdiff, `[[`, 6)))
df_S_data_and_sim_diff <- data.frame(dataS =c(S_data_diff,Sn_data$Sn), group=rep(c('Data','Sim'),times=c(length(S_data_diff),length(Sn_data$Sn))))
df_T_data_and_sim_diff <- data.frame(dataT =c(T_data_diff,Tn_data$Tn), group=rep(c('Data','Sim'),times=c(length(T_data_diff),length(Tn_data$Tn))))

psnS_data_diff <- ggplot(df_S_data_and_sim_diff, aes(x=dataS, color=group)) + 
  labs(x = unname(TeX("$S$")), y = "Density")+
  theme_classic() +
  geom_density() 
psnS_data_diff

psnT_data_diff <- ggplot(df_T_data_and_sim_diff, aes(x=dataT, color=group)) + 
  labs(x = unname(TeX("$T$")), y = "Density")+
  theme_classic() +
  geom_density() 
psnT_data_diff

x <- df2[df2$TimeDifference ==uniques[30],]
y<- df2[df2$TimeDifference ==uniques[20004],]
nrow(y)
length(uniques)

ggplot(data=x,aes(Value,Forecasts))+
  labs(x = unname(TeX("Actuals")), y = "Forecasts")+
  geom_abline(intercept = 0,slope=1,color="blue") +
  geom_line()

ggplot(data=y,aes(Value,Forecasts))+
  labs(x = unname(TeX("Actuals")), y = "Forecasts")+
  geom_abline(intercept = 0,slope=1,color="blue") +
  geom_line()

nrow(x)

#Creating the Copula
data <- timedifflist[[1000]]

X <- data$Value
Y <- data$Forecast

plot(X, Y, main = "Scatter Plot", xlab = "Value", ylab = "Forecast")

Fn <- ecdf(X)
Gn <- ecdf(Y)

U_hat <- Fn(X)
V_hat <- Gn(Y)

W_hat <- cbind(U_hat,V_hat)
cop <- empCopula(W_hat,smoothing="beta")


# Create a matrix of the bivariate copula densities
z <- matrix(0, nrow = 100, ncol = 100)
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = 100*100, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar
U <- seq(0,1,length.out=100)
k <- 0
for (i in 1:100) {
  for (j in 1:100) {
    z[i, j] <- dCopula(copula =cop, u =c(U[i], U[j]))
    k <- k+1
    setTxtProgressBar(pb,k)
  }
}
close(pb)

# Create a heatmap
require(lattice)
library("viridis")           # Load
heatmap <- levelplot(z, col.regions = magma(16), xlab = "U", ylab = "V",
                     panel = function(...) {
                       panel.levelplot(...)
                       panel.xyplot(U_hat*100, V_hat*100,alpha = 0.25, col = "orange", pch = 16, scales = list(x = list(relation = "same"), y = list(relation = "same")))
                     })


z_df <- as.data.frame(z)
colnames(z_df) <- U
z_df <- mutate(z_df,U=U)

z_pivot <- pivot_longer(z_df,cols=paste0(U))
colnames(z_pivot) <- c("U","V","z")
z_pivot <- mutate(z_pivot,V=as.numeric(V))
some_df <- cbind(U=U_hat,V=V_hat)

library(kdensity)
fn <- kdensity(X)

k <- function(x,y){
  U <- Fn(x)
  V <- Gn(y)
  dCopula(c(U,V),cop)*fn(x)
}

x_vals <- seq(0, 14000, by = 10)  # Adjust the sequence to start from 0
y_val1 <- map_dbl(x_vals, ~ k(.x, 1000))
y_val2 <- map_dbl(x_vals, ~ k(.x, 2500))
y_val3 <- map_dbl(x_vals, ~ k(.x, 5000))
y_val4 <- map_dbl(x_vals, ~ k(.x, 7500))
y_val5 <- map_dbl(x_vals, ~ k(.x, 10000))
y_val6 <- map_dbl(x_vals, ~ k(.x, 12500))

# Create a data frame
df <- data.frame(x = rep(x_vals, 6),
                 y = c(y_val1, y_val2, y_val3, y_val4,y_val5,y_val6),
                 group = rep(c("1000","2500", "5000","7500", "10000", "12500"), each = length(x_vals)))

# Convert "group" to a factor with the desired order
df$group <- factor(df$group, levels = c("1000","2500", "5000","7500", "10000", "12500"))

# Plot using ggplot2
ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line() +
  geom_vline(xintercept = c(1000,2500, 5000,7500, 10000, 12500), 
             linetype = "dashed",color = c("red", "brown", "darkgreen", "darkcyan", "blue", "deeppink")) +
  labs(x = "Forecasts",
       y = "Density") +
  guides(color = guide_legend(title = "Actuals"))   # Optional: Provide a custom legend title




