#Double Beastly Log Problem Generator
#by Paul de Barros
#Last updated 2014-12-19

#This program attempted to create problems of the form L^3 - 3L^2 - 4L +12 = 0, 
#where L is log {ax+b} (cx+d), for integers a,b,c,d,
#where {ax+b} is the base of the log and (cx+d) is the argument.
#Factor by grouping allows 3 separate equations to come from this,
#leading to one quadratic and 2 cubics, where the cubics should be amenable to factor-by-grouping.
#This turned out not to be possible.  Even using a large set of parameters, no valid problems were generated.


#x holds all of the possible sets of the first 4 parameters that can be used to generate a problem
#y holds repeats of x for each value of the final parameter, s2

library(gtools) #this is needed for the permutation function

n = 50 #set the max absolute value of the first 4 parameters (p1, q1, r1, s1)
m = 50 #set the number of values for parameter s2

x1 = permutations(n,4, repeats.allowed = TRUE)              #generate all permutations of the first 4 parameters with r2, s2 > 0
x2 = x1 * c(rep(1,3*dim(x1)[1]),rep(-1,dim(x1)[1]))         #generate all permutations of the first 4 parameters with s2 < 0
x = rbind(x1, x2)                                           #combine for all permuations with r2 >0
x2 = x * c(rep(1,2*dim(x)[1]),rep(-1,dim(x)[1]),rep(1,dim(x)[1])) #generate all permutations of the first 4 parameters with r2 < 0
x = rbind(x, x2)                                            #combine for all permuations
x = data.frame(x)                                           #convert it to a data frame to make it easier to use
names(x) = c("p2","q2","r2","s2")                           #name the columns
x$a = (x$q2^2 * x$r2)^(1/3)                                 #calculate a
x$b = x$q2^2 * x$s2 / (3*x$a^2)                             #calculate b
x$c = x$p2^2 * x$r2 + 3 * x$a * x$b^2                       #calculate c
x$d = x$p2^2 * x$s2 + x$b^3                                 #calculate d


#eliminate rows with non-integer a,b,c,d
x = x[which(x$a == round(x$a)),]
x = x[which(x$b == round(x$b)),]
x = x[which(x$c == round(x$c)),]
x = x[which(x$d == round(x$d)),]

#calculate the ratio of r1 to s1 and grab the numerator and denominator of the reduced fraction
x$Num = MASS:::.rat((x$a^2*x$c)/(x$a^2*x$d+2*x$a*x$b*x$c))$rat[,1]       #this grabs the numerator
x$Den = MASS:::.rat((x$a^2*x$c)/(x$a^2*x$d+2*x$a*x$b*x$c))$rat[,2]       #this grabs the denominator

#initialize the new expanded data frame
y = data.frame(numeric(0))
for (i in 1:(dim(x)[2]-1)){
  y = cbind(y,data.frame(numeric(0)))
}

#fill y with repeats of x
for (i in 1:m){
  y = rbind (y,x)  
}




mvec = rep(1:m, each = dim(x)[1])   #create the multiplier that will be used to calculate s1
y$s1 = y$Den * mvec                 #calculate s1
y$r1 = y$Num * y$s1 / y$Den         #calculate r1
y$p1 = sqrt((-1*y$b^2*y$c - 2*y$a*y$b*y$d)/y$r1)   #calculate p1
y = y[which(y$p1 == round(y$p1)),]
y$q1 = sqrt(y$a^2*y$c/y$r1)                        #calculate q1
y = y[which(y$q1 == round(y$q1)),]


