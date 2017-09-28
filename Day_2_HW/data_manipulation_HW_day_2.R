############ 0 #############
#setwd
setwd("~/Documents/UCLA/Rbootcamp/Bootcamp_WD")

###########################
########### 1 #############
###########################

########### a #############
# Load a data set as a table
#this dataset has a header 
#SNPs data set saved as snpsDataFrame
snpsDataFrame=read.table("hapmap_CEU_r23a_chr2_ld-2.txt", header=TRUE)

# Because the data are really just a large numeric matrix, we convert the 
#dataframe to a matrix saved as snps
snps=as.matrix(snpsDataFrame)

#this taken from class notes:
#function to calculate chi sq for genetic data set 
#renamed func
comp_chisquare=function(x){
        freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))) 
        #cal freq of an allele by counting its presence / 2*all counts since diploid
        cnt0=sum(x==0,na.rm=TRUE) #count the homrec indivs
        cnt1=sum(x==1,na.rm=TRUE) #count the het indivs 
        cnt2=sum(x==2,na.rm=TRUE) #count the homdom indivs
        obscnts=c(cnt0,cnt1,cnt2) #vector with all the counts
        #print(obscnts)
        n=sum(obscnts) #sum all the counts
        #here we use the built-in function for the chi-sq distribution:
        exp_probs=c((1-freq)^2,2*freq*(1-freq),freq^2) 
        #note, here we don't multiply by n
        #this is the expectation from HW
        chisq<-chisq.test(obscnts,p=exp_probs, correct = FALSE)$statistic
        #now were testing our obsv vs expectted using chi square to see if diff
        return(chisq)
}

#for testing
#testSNP=snps["rs6717613_A",]
#comp_chisquare(testSNP)
#returns a chi sq value

################# don't think this is needed
#make a place to store this values 
#pvals <- matrix(NA, nrow = 1, ncol = (nrow(snps)))

#now we want to calc a chi sq for each allele aka each row
#so we'll use apply and keep dim 1 
#use apply to run chi sq on each row to generate chi sq values
X2vals <- apply(snps, 1, comp_chisquare)
#this will giving warnings thst approzimation may be incorrect, but it's okay

#calculate p values and store
pvals <- pchisq(X2vals,1,lower.tail=FALSE)


############# b ###################
#porp of p's < 0.05
#pvals < 0.05 is logical so gives T/F so sum gives all the T's
#length(pvals) counts total # of pvals
sum(pvals < 0.05) / length(pvals)
#0.04509218

#porp of p's < 0.01
sum(pvals < 0.01) / length(pvals)
# 0.01021425

#porp of p's < 0.001
sum(pvals < 0.001) / length(pvals)
#0.00124564

########### c #############
#calc how many pvals and store in num_pval
num_pval <- length(pvals)

########### d ############
#calculating exp pvals

#smallest is 1 / (total # pvals)
#next is 2 / (total # pvals)
#..... 1 -> (total # pvals) / (total # pvals)
#so we'll need a loop going from 1 to total # pvals
#and we'll need a place to store those numbers

#matrix to store exp p values 
exp_pvals <- matrix(NA, nrow = 1, ncol = num_pval)

#for loop to generate and store all the exp p vals 
for(ii in 1:num_pval){
        exp_pvals[ii] <- ii / num_pval 
        #divide the index by total number pvals
        #store in exp_pvals location based on index
}

############### e #################
#using sort func to sort obs p vals in ^ order
#default in sort is to put them in ^ order
#store in sort_pvals
sort_pvals <- sort(pvals)

############## f ##################
#take -log10 of our sorted and exp p vals and store them
#use the log bc what we care most about is really freaking small

log_sort_pvals <- -log10(sort_pvals)
log_exp_pvals <- -log10(exp_pvals)

############## g #################
#make the plot, exp on x, obs on y
plot(log_exp_pvals, log_sort_pvals, col = 1, ylab = "-log10 Observed P Value",
     xlab = "-log10 Expected P Value", ylim = c(0,5), xlim = c(0,4))

############## h ##############
#add a line to most recent plot
#intercept 0, slope 1, color red
abline(0, 1, col = 2)

############# i ##############
#it does.


##############################
############# 2 ##############
#############################

############# a #############
#open file and save as DF named zz
zz <- read.table("pheno.sim.2014-2.txt", header=TRUE)

############# b ##############
#use quantile to find the 1st quantile of col 2 of zz (gluc lvl)
quantile(zz[ , 2], 0.25)

################# c ######################
#use quantile to find the 3rd quantile of col 2 of zz (gluc lvl) 
quantile(zz[ , 2], 0.75)

############## d #################
#plot the density of gluc vals (col 2 of zz)
plot(density(zz[ , 2]), xlab = "Glucose Blood Levels (mmol/L)", 
     main = "Density of Glucose Blood Levels")

#add red vert line at 1st quart
abline(v=quantile(zz[ , 2], 0.25),lty=1,lwd=2,col=2)

#add blue vert line at 3rd quart
abline(v=quantile(zz[ , 2], 0.75),lty=1,lwd=2,col=4)

#add a legend for shits and giggles 
#red is 1st quartile
#blue is 3rd quartile
legend(8.5, 0.3,c("1st Quartile","3rd Quartile"),col=c(2,4), pch=16, cex = 1)

