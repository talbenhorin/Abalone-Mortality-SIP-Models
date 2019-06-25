# Simple exponential decay functions to quantify abalone mortality from experimental data
# Data from Moore et al. (2011) 

rm(list=ls(all=TRUE)) #clears workspace

abmort <- read.csv("https://raw.githubusercontent.com/talbenhorin/Abalone-Mortality-SIP-Models/master/mooredata.csv", fill = FALSE, header = TRUE) 

x <- abmort$x.elnino # time in years 
y <- abmort$y.elnino # proportion of the initial abalone population alive

mu <- 0.15 #abalone natural mortality rate (from Tegner et al. 1989)
df <- data.frame(x, y) #create data frame from your x y data
m <- nls(y ~ exp(-(mu+r)*x), data = df, start = list(r = 0.1)) #nonlinear least squares fit
summary(m) #summary of output m

# plot x y data and the fitted function to assess fit
plot(x,y)
t <- seq(from = 0, to = 1, length = 50)
lines(t, predict(m, list(x = t)), col = "black")
