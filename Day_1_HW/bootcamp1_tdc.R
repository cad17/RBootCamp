########### 1 ##############
 for(i in 1:9) {
         #a loop that indents 8 times then prints an *
         
         if(i < 9) {
                 cat("\n") #if i < 9 just indent
         }
         if(i == 9) {
                 print("*") #if i == 9 print an * note "" needed 
         }
 }

########### 2 ###############
### this works but is ugly as hell 
star <- vector(length = 9) #make star an empty vector
previous <- NULL #make previous an empty vector
for(i in 1:9) {
        if (i == 1){ #for first loop just inputting an *
                star[i] <- "*"
                previous <- star[i] #setting start of previous to just *
        }
        if(i < 9 & i > 1){ #now paste together previous * using &
        star[i] <- "*" #set that given thing to *
        previous <- paste(previous, star[i], sep="&")
        }
        #paste things together
        #with & in between
        if(i == 9){
                star[i] <- "*" #set that given thing to *
                previous <- paste(previous, star[i], "*", sep="&") 
                #note here we add another * to get 10 total 
                print(previous) #when we get to the end print the string
                }
}

############## 3 #####################
#dogs = 10, 11, 12, 13, 14, 15

#meatloaf = -4, -9, -15, -22, -30

#bubbles = 12, -1, -2, -3, -4

############## 4 ###################
###you can use the if statement with the modulus operator to conditionally perform operations
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
        #if year is divisible by 4 give presidential and congressional message
        if(years[ii] %% 4 == 0){
                cat(years[ii], 'Hooray, presidential and congressional elections!', sep = '\t', fill = T)
        }
        #if year is divisible by 2 but not 4 give congressional election
        if(years[ii] %% 4 > 0 & years[ii] %% 2 == 0) {
                cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
        }
        
}

############# 5 ###################
bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);

#Now look at the error message the following lines of code produce. 
#Can you think of a way to modify this loop so that the loop will compund the interest?

interestRate <- 0.0125;
compounded <- NULL #create empty vector compounded 
for (i in 1:length(bankAccounts)) {
        compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; }

#HINT: variables must be initialized before you can perform operations on them
#HINT 2: look at the rep() function and see if you can use that to initialize a variable that will help you.

############# 6 ###################
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

for (j in 1:5) { #this will repeat it for 5 years
        for (i in 1:length(bankAccounts)) {
                ####for each student we are modifying base income then doing interest 
                #step 1 modify bankAccounts so that amounts reflect income and expenses
                bankAccounts[i] <- bankAccounts[i] - house[i] - food[i] - fun[i] + income[i]
                #step 2 get calculate interest and add to accounts from step 1
                bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i]
                #you can actually use the line you have already written if you
                #modify amounts in bankAccounts directly in step 1
        }
        print(bankAccounts)
}

############# 7 ###################
years <- 2015:2020
bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

for (j in 1:length(years)) { #this will repeat it for 5 years
        
        if(years[j] %% 2 == 0){ #even number years unchanged
                for (i in 1:length(bankAccounts)) {
                        ####for each student we are modifying base income then doing interest 
                        #step 1 modify bankAccounts so that amounts reflect income and expenses
                        bankAccounts[i] <- bankAccounts[i] - house[i] - food[i] - fun[i] + income[i]
                        #step 2 get calculate interest and add to accounts from step 1
                        bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i]
                        #you can actually use the line you have already written if you
                        #modify amounts in bankAccounts directly in step 1
                }
        print(bankAccounts)
        }
        
        if(years[j] %% 2 == 1){ #odd number years unchanged
                for (i in 1:length(bankAccounts)) {
                        #students 1 and 3 get 5K, lucky bastards
                        bankAccounts[c(1,3)] <- bankAccounts[c(1,3)] + c(5, 5)
                        ####for each student we are modifying base income then doing interest 
                        #step 1 modify bankAccounts so that amounts reflect income and expenses
                        bankAccounts[i] <- bankAccounts[i] - house[i] - food[i] - fun[i] + income[i]
                        #step 2 get calculate interest and add to accounts from step 1
                        bankAccounts[i] <- interestRate*bankAccounts[i] + bankAccounts[i]
                        #you can actually use the line you have already written if you
                        #modify amounts in bankAccounts directly in step 1
                }
                print(bankAccounts)
        }
        
        
}

################## 8 ###################
num_sum <- 0 #make num_sum empty vector
num <- 0 #make num empty var
while(num < 17){
        num <- num + 1 #add 1 to num 
        num_sum <- num_sum + num 
        print(num_sum)
}

print(num_sum)
        

################## 9 ###################
num_size <- function(num){
        if (num <= -1){
                print("small")
        }
        if (num > -1 & num < 1){
                print("medium")
        }
        if (num >= 1){
                print("big")
        }
}

#################### END ################