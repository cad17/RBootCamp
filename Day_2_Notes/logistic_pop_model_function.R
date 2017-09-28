#############################
#Use the psudeo code structur to actually write code to model pop growth

#1) Setup - load pkgs, set wd, etc 


#2) Input data, param vals, initial cond

log_pop_model <- function(NN, rr, ttmax, KK){
        #pass initial params as arguments 
        #NN = initial pop
        #rr = growth rate
        #ttmax = how many years
        #KK = carrying capacity

#3) Preform calcs
#make a for loop that goes 10 years
#in loop make new N based on previous N*R
#store each value in pop vector
        
        pop <- matrix(NA, nrow=1, ncol = ttmax+1)
        #initialiaze a vector to stor pop size over years
        pop[1] <- NN
        #set initial pop to NN
        
        #this is the meat of the function
        #calculats new pop and stores it in matrix pop
        for(tt in 1:ttmax){
                pop[tt+1] <- pop[tt] * (1 + rr * (1-(pop[tt]/KK) ) ) 
        }
#note that * are needed to tell R to multiply
#based on logistic growth model

#4) Display results by plotting, saving, or showing
#plot pop matrix v t

        plot(1:(ttmax+1), pop, xlab = "Years", ylab = "Population Size")
}
