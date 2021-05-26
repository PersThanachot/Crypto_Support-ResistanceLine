library(tidyverse)
library(ggpmisc)
theme_set(theme_classic())

p = ggplot(df2, aes(x = No, group = 1)) + 
  geom_line(aes(y=High), colour = "blue") +
  stat_peaks(aes(y=High),colour = "red") +
  geom_line(aes(y=Low), colour = "green") +
  stat_valleys(aes(y=Low),colour = "red")
#p

# Get locations of peaks found by stat_peaks
pb = ggplot_build(p)
peaks = pb$data[[2]] %>% 
  arrange(x)

valleys = pb$data[[4]] %>% 
  arrange(x)

# Fit a spline function to the peaks and get a data frame of points so that 
#  we can add the spline fit to the plot
#peaks.spline = as.data.frame(spline(peaks$x, peaks$y, n=6*nrow(peaks)))

# Add spline to plot
#p + geom_line(data=peaks.spline, aes(x, y), colour="red")

valleys$No <- row.names(valleys)
valleys$No <- as.numeric(valleys$No)

reg <- lm(y ~ No,valleys)


#abline(a = reg$coefficients[1],reg$coefficients[2]*nrow(valleys), col = "red")

#plot(x = nrow(valleys) ,y = reg$coefficients[1]+reg$coefficients[2]*x )

#reg$coefficients[1]+reg$coefficients[2]*nrow(valleys)

segments(x0 = 1, 
         y0 = reg$coefficients[1] , 
         x1 = nrow(df2)*3,
         y1 = reg$coefficients[1]+reg$coefficients[2]*nrow(valleys),
         col ="red") 

#points(x = 1 , y = 32 ,col ="red" )
