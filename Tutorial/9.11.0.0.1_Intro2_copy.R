X <- read.csv('ChlorellaGrowth.csv',comment.char='#')
# using '#' as a comment character allows us the CSV file to be self-documenting

LogLight = log(light) #taking log of Light

par(cex=1.5,cex.main=0.9)
plot(rmax~LogLight,data=X,xlab="Log light intensity (uE/m2/s)"
     , ylab="maximum growth rate (1/d)",pch=16)  #changed be LogLight
# xlab and ylab are x and y axis labels, pch is "plotting character"
# cex is 'character expansion' - cex=1.5 increases symbol & label sizes by 50%
# cex.main sets the character expansion for the main title of the plot 

title(main="Data from Fussmann et al. (2000) system")
fit <- lm(rmax~LogLight) #LogLight instead of Light
summary(fit); abline(fit) 

# Next we get the regression equation to 'display itself' on the graph
c1 <- round(fit$coef[1],digits=3) 	# intercept	
c2 <- round(fit$coef[2],digits=3) 	# slope
text(50,3,paste("rmax=",c1,"+",c2,"light")) 

# You can use ?round, ?text and ?paste to read about these commands
# for working with plots  
