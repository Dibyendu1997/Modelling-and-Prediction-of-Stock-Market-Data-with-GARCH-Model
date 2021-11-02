# Modelling-and-Prediction-of-Stock-Market-Data-with-GARCH-Model
<h1 align="center">
  <img src="https://cdn-images-1.medium.com/max/1024/0*Y6j2EdSa6TLHu_bW" width="40%">
</h1>


# Introduction
Time series of returns from financial instruments usually exhibit nonlinearities. There is strong empirical evidence that a large number of time series
from finance and economics show some stylized facts such as clusters of highly variable observations followed by clusters of observations with low variability
and strong autocorrelations either in the series or its squares. In this Project we examine some of the models proposed to account for these features. In par-
ticular, we consider heteroskedastic time series models where the conditional variance given the past is no longer constant

In This Project We Use ARMA-GARCH Model for Modeling Four different type of Stock Markets .   

# Data Collection
We Use NIFTY 50 , NIFTY NEXT 50 , NIFTY MIDCAP ,NIFTY SMALL CAP for this project .We use Data from March 23, 2020 to Sep 24, 2021 and use this data for predecting stock market price for next 20 days. We Collect our data from [Yahoo! Finance](https://finance.yahoo.com/quote/%5ENSEI/history/) . 
# Model NIFTY 50 Data 

Import Necessary libraries
  <details>
  <summary>Click to expand!</summary>
 
  ```r
    library(randtests)
    library(forecast)
    library(urca)
    library(aTSA)
    library(ggplot2)
    library(tsoutliers)
    library(rugarch)
    library(sgt)
    library(quantmod) 
    library(xts)
    library(corrplot)
  ```
  </details>
  
  Plot NIFTY50 Data
  
  <details>
  <summary>Click to expand!</summary>
 
  ```r
    getSymbols("^NSEI" ,from = "2020-03-23" ,to = "2021-09-24")
    chartSeries(NSEI["2021-09"])
  ```
  </details>
  
![image](https://user-images.githubusercontent.com/76222216/139868150-96df12ed-aab0-45c4-9524-641469e75166.png)


  <details>
  <summary>Click to expand!</summary>
 
  ```r
  return <- log(tso[2:length(tso)]/(rev(nfty50$Close)[-length(nfty50$Close)]))
return_tso <- zoo(return, rev(nfty50$Date)[-1])
#ret = log(rev(nfty50$Close))
#return = -(ret[1:(length(ret)-1)] - ret[2:length(ret)])
Return=CalculateReturns(tso, method = 'log')
ggplot() + geom_line(mapping = aes(x = 1:length(return), y = return, colour = 'NIFTY 50')) +
  #  geom_line(mapping = aes(x = 1:length(returnNftnxt50), y = returnNftnxt50, colour = 'NFTY NEXT 50')) +
  scale_colour_manual('Stock', breaks = c("NIFTY 50"), # "NFTY NEXT 50"),
                      values = c("yellow")) + #, "blue")) +
  scale_y_continuous("Log Return of NIFTY 50 Stock Price") +
  labs(title = "Log Returns of the Closing Prices of NIFTY 50", y = "Log Return of the Stock Prices", x = "Time points")+theme(panel.background = element_rect(fill = "#000033",  colour = "white",size = 0.5, linetype = "solid"),panel.grid.major = element_line(colour = "grey", linetype = "dotted"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))


  ```
  </details>

![image](https://user-images.githubusercontent.com/76222216/139870784-b5384cc2-6ffd-4860-9d1d-9725348a24a6.png)

  <details>
  <summary>Click to expand!</summary>
 
  ```r
   chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
  ```
  </details>


![image](https://user-images.githubusercontent.com/76222216/139872995-51cb8049-8e92-45b5-94ae-24ba671030cf.png)

  <details>
  <summary>Click to expand!</summary>
 
  ```r
x = rep(0, as.integer(length(return)/22))
for (i in seq(10,(length(return)),22)){
  m = Return[i-10:i+11]
  x[i] = sd(m)
}

acf2(x,max.lag=21)
a<- ggAcf(na.omit(as.vector(x)), col='red',main='Acf of volatility of NIFTY 50');
p<- ggPacf(na.omit(as.vector(x)),col='steelblue',main='PAcf of volatility of NIFTY 50')
grid.arrange(a,p, ncol = 2, nrow = 1)

  ```
  </details>
  
  ![image](https://user-images.githubusercontent.com/76222216/139876341-6016d60d-17ae-488e-b319-a354294ece91.png)

# Augmented Dickey Fuller Test for log return

 ```r
Value of test-statistic is: -1.0497 6.1874 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.44 -2.87 -2.57
phi1  6.47  4.61  3.79

```

# Augmented Dickey Fuller Test for square log return

 ```r
Value of test-statistic is: -15.2982 117.053 

Critical values for test statistics: 
      1pct  5pct 10pct
tau2 -3.44 -2.87 -2.57
phi1  6.47  4.61  3.79
```

  <details>
  <summary>Click to expand!</summary>
 
  ```r
  a<- ggAcf(na.omit(as.vector(return)), col='red',main='Acf of Log Return of NIFTY 50')
  p<- ggPacf(na.omit(as.vector(return)),col='steelblue',main='PAcf of Log Return of NIFTY 50')
  grid.arrange(a,p, ncol = 2, nrow = 1)
 ```
  </details>
  
  ![image](https://user-images.githubusercontent.com/76222216/139878778-59b8a7fc-eb54-4bb3-9003-70189fd21db1.png)


  
  
   <details>
  <summary>Click to expand!</summary>
 
  ```r
  a<- ggAcf(na.omit(as.vector(Return)^2), col='red',main='Acf of Square Absolute Return of NIFTY')
  p<- ggPacf(na.omit(as.vector(Return)^2),col='steelblue',main='PAcf of Square Absolute Return of NIFTY')
  grid.arrange(a,p, ncol = 2, nrow = 1)

  ```
  </details>
  
  ![image](https://user-images.githubusercontent.com/76222216/139878855-e876fedf-30fe-4529-892c-8c79740d8fb8.png)

# Stylized Facts of Financial Data
 Distributions of Returns is not normal.
 Absence of significant auto correlation in returns.
 Slowly decreasing auto correlation in squared or absolute returns.
 Volatility clustering.
 
 # Volatility clustering
  
  ![image](https://user-images.githubusercontent.com/76222216/139879477-07b65fa2-890d-4e1b-a895-474855224001.png)

  
   <details>
  <summary>Click to expand!</summary>
 
  ```r

  ```
  </details>
  
  
  
  
   <details>
  <summary>Click to expand!</summary>
 
  ```r

  ```
  </details>
  
  
  

# Result and Analysis

