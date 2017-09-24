############# 1 #############
#make a function to randomly generate heights of n people and with give mean/sd
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
        h_mean <- mean(heights) #calc and store mean of heights
}

########## 3 #############
#modifying to now print mean
get_heights <- function(pop_size = 100){
        #pop_size is how many samples to take
        #take random numbers with mean of 69" and sd of 10" 
        #stored in variable heights
        heights <- rnorm(pop_size, mean = 69, sd = 10)
        #str(heights) #for testing print this
        h_mean <- mean(heights) #store mean of heights
        h_mean #print mean of heights
}

############ 4 ##############
#run 1000 simulations of sampling 100 people
mean_heights_100 <- NULL #make empty vector
for(i in 1:1000){
        #starting with 10 to test
        mean_heights_100[i] <- get_heights(100) 
        #run get heights for 100 people 
        #and store result in mean_heights 100
}

############ 5 ##############
#run 1000 simulations of sampling 1000 people
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
        as.integer(min(min(mean_heights_100), min(mean_heights_1000)) - 1)

#find the max between both sets
max_height <- 
        as.integer(max(max(mean_heights_100), max(mean_heights_1000)) + 1)


#setting the bins
bins <- seq(min_height, max_height, by = 1)

#collecting counts based on our bins
counts_h100 <- hist(mean_heights_100,breaks=bins)$counts
counts_h1000 <- hist(mean_heights_1000,breaks=bins)$counts

par(mfrow=c(1,1), mar=c(4, 4, 3, 2)) #sets plotting area and
#margins

pdf(file="plotting_hw_hist_tdc.pdf", width=6,height=6); #open the file

barplot(rbind(counts_h100,counts_h1000),col=c(2,4),beside=T,names.arg=
                seq(min_height, (max_height - 1), by = 1),xlab="Average Height (in)",ylab="Count")
#rbind used so samples will be compared at each bin
#100 = red(2), 1000 = blue (4)
#besides arg say bars show side by side v stacked
#names.arg sets labels of each pt on x axis
#x/y lab are the labels for the axis 


#adding the legend
legend(1,400,c("n = 100","n = 1000"),col=c(2,4), pch=16, cex = 1.5)
#(x pos of top L, y pos of top L, labels, label colors, shapes, legend size)

dev.off() #close file
