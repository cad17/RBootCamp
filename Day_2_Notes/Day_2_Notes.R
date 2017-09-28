#############################
#Use the psudeo code structur to actually write code to model pop growth

#1) Setup - load pkgs, set wd, etc 


#2) Input data, param vals, initial cond
#set N to 100
#set R to 1.05
#initialize a vector to stor pop size over years
NN <- 100
RR <- 1.05
pop <- NULL
years <- 1:10
#3) Preform calcs
#make a for loop that goes 10 years
#in loop make new N based on previous N*R
#store each value in pop vector

for(tt in years){
        NN <- NN * RR
        pop[tt] <- NN
}

#4) Display results by plotting, saving, or showing
#print N at 10 years
#plot pop vector v t

plot(pop~years, xlab = "Years", ylab = "Population Size")
