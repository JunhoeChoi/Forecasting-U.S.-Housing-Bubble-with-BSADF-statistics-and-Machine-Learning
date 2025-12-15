library(psymonitor)  # For testting for bubble monitoring
library(ggplot2)     
library(knitr)       

hpi      <-read.csv("/Users/choejunhoe/Desktop/Housingbubbleproject/MLProject/nominal_hpi.csv")
hpi$DATE <- as.Date(hpi$DATE)

y        <- hpi$rent
obs      <- length(y)
r0       <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0*obs) # set minimal window size
dim      <- obs - swindow0 + 1
IC       <- 2 # This implies I am using BIC to select the number of lags
adflag   <- 6 # set the maximum number of lags to 6
year       <- 2 # Two years
Tb       <- 4*year + swindow0 - 1 # Set the control sample size
nboot    <- 10000

# estimate the PSY test statistics sequence
bsadf          <- PSY(y, swindow0, IC, adflag)

# simulate critical values via wild bootstrap.
quantilesBsadf <- cvPSYwmboot(y, swindow0, IC, adflag, Tb, nboot, nCores = 2) 
# Note that the number of cores is arbitrarily set to 2.

monitorDates <- hpi$DATE[swindow0:obs]
quantile95   <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
ind95        <- (bsadf > t(quantile95[2, ])) * 1
periods      <- locate(ind95, monitorDates) # Locate crisis periods

bubbleDates <- disp(periods, obs)
kable(bubbleDates, caption = "Bubble and Crisis Periods in the U.S. Housing Market","latex")

ggplot() + 
  geom_rect(data = bubbleDates, aes(xmin = start, xmax = end, 
                                    ymin = -Inf, ymax = Inf), alpha = 0.5) + 
  geom_line(data = hpi, aes(DATE, rent)) +
  labs(title = "Housing Bubble Period with Rent Index",
       subtitle = "January 1976 - October 2021",
       caption = "Notes: The solid
line is the Rent Index and the shaded areas are the periods where
the PSY statistic exceeds its 95% bootstrapped critical value.", 
       x = "Year", y = "rent index") 
n <-length(hpi$hpi)-length(ind95)
o_rent= rep(0,n)
bubble_rent=c(o_rent, ind95)
rent_bubble<-cbind(hpi,bubble_rent)

write.csv(rent_bubble,"/Users/choejunhoe/Desktop/Housingbubbleproject/MLProject/rent_bubble.csv")

