#############################
#Now we'll use our meodel to look at how our mode is impacted by 
#a given param
#we'll look at pop at year 5 as impacted by rr
#1) Setup - load pkgs, set wd, etc 

#first set other paramaters to hold
NN <- 100
ttmax <- 5
KK <- 250
rr_values <- seq(-1, 1, by = 0.25)
#2) Input data, param vals, initial cond

five_year_pop <- matrix(NA, nrow = 1, ncol = length(rr_values))

################
#this is throwing error say ttmax is missing

for(ii in 1:5){
        five_year_pop[ii] <- log_pop_model(rr = rr_values[ii])
}



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
        
        
        print(pop[tt+1])
        
}
