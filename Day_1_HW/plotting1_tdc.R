############# 1 #############
get_heights <- function(pop_size){
        #pop_size is how many samples to take
        #take random numbers with mean of 69 in and sd of 10 in
        #stored in variable heights
        heights <- rnorm(pop_size, mean = 69, sd = 10)
        #str(heights) #for testing print this
}

############ 2 ############
#modifying to now calculate mean

get_heights <- function(pop_size){
        #pop_size is how many samples to take
        #take random numbers with mean of 69 in and sd of 10 in
        #stored in variable heights
        heights <- rnorm(pop_size, mean = 69, sd = 10)
        #str(heights) #for testing print this
        h_mean <- mean(heights) #calc mean of heights
}

########## 3 #############
#modifying to now print mean

get_heights <- function(pop_size = 100){
        #pop_size is how many samples to take
        #take random numbers with mean of 69 in and sd of 10 in
        #stored in variable heights
        heights <- rnorm(pop_size, mean = 69, sd = 10)
        #str(heights) #for testing print this
        h_mean <- mean(heights) #print mean of heights
        h_mean #print mean of heights
}

############ 4 ##############
mean_heights_100 <- NULL #make empty vector
for(i in 1:1000){
        #starting with 10 to test
        mean_heights_100[i] <- get_heights(100) 
        #run get heights for 100 people 
        #and store result in mean_heights 100
}

############ 5 ##############
mean_heights_1000 <- NULL #make empty vector
for(i in 1:1000){
        #starting with 10 to test
        mean_heights_1000[i] <- get_heights(1000) 
        #run get heights for 100 people 
        #and store result in mean_heights 100
}

########### 6 ##############

#find the min between both sets
min_height <- 
        as.integer(min(min(mean_heights_100), min(mean_heights_1000)) - 2)

#find the max between both sets
max_height <- 
        as.integer(max(max(mean_heights_100), max(mean_heights_1000)) + 2)


#setting the bins
bins <- seq(min_height, max_height, by = 1)

#collecting counts based on our bins
counts_h100 <- hist(mean_heights_100,breaks=bins)$counts
counts_h1000 <- hist(mean_heights_1000,breaks=bins)$counts

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and
#margins

#############
############
###########
#Okay till here, taking a break

barplot(rbind(counts_h100,counts_h1000),col=c(2,4),beside=T,names.arg=
                seq(min_height, (max_height-1) by = 1),xlab="Value",ylab="Count")
