#############################
#Use the psudeo code structur to actually write code to model pop growth

#1) Setup - load pkgs, set wd, etc 


#2) Input data, param vals, initial cond
#set N to 100
#set R to 1.05
#initialiaze a vector to stor pop size over years
NN <- 100
rr <- 1.05
ttmax <- 10
pop <- matrix(NA, nrow=1, ncol = ttmax+1)
pop[1] <- NN
K <- 1000
#3) Preform calcs
#make a for loop that goes 10 years
#in loop make new N based on previous N*R
#store each value in pop vector

for(tt in 1:ttmax){
        pop[tt+1] <- pop[tt] * (1 + rr * (1-(pop[tt]/K) ) ) 
}
#note that * are needed to tell R to multiply
#based on logistic growth model

#4) Display results by plotting, saving, or showing
#print N at 10 years
#plot pop vector v t

plot(1:(ttmax+1), pop, xlab = "Years", ylab = "Population Size")
