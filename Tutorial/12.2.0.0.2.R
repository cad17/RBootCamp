a <- 1.1
b <- 0.001
N <- 2 #N starts at 2
conv <- F
while (!conv) {
        N.new <- a*N/(1+b*N) #set new N based on last N
        conv <- N == N.new #are N.new and N the same, 
        #have we approached the limit
        N <- N.new #set the N to be the N.new value
}