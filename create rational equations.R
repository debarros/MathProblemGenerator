#Beastly Rational Equation Problem Creator

# This code will allow you to create problems that result in the form (x^2 - a^2b^2)(x + d)=0, [form 1]
# and can be rewritten as  x^3 + (d+1)x + long*x = x^2 + medium*x + large [form 2]
# or as x(x+q)(x+r) = (x + fac1)(x + fac2) [form 3]
# which can then be reorganized and unsimplified as a rational equation 


q <- 1
a <- 1
b <- 1
d <- 1
r <- 1

M <- data.frame(0,0,0,0,0,0,9)
M
names(M) <- c("a","b","d","q","r","large", "mag")

i <- 1 # initialize the row of the matrix

q.range = c(-30:-1, 1:30)
a.range = 2:30
M[i,]

for(q in q.range){
  r.range = q:30
  r.range = r.range[r.range != 0]     # don't want r to be 0
  r.range = r.range[r.range != 1 - q] # don't want d to end up being 0
  for(r in r.range){
    d <- q + r - 1                    # calculate d
    for (a in a.range){
      if((q*r - a*d)/(a - a^2) > 0){              # don't want b to end up being a complex number
        b <- sqrt((q*r - a*d)/(a - a^2))          # calculate b
        if((round(b) == b) & (b^2 != abs(b))){    # if b is an integer other than -1, 0, or 1
          if ((b < 100000) & length(unique(abs(c(a,b,d)))) == 3){ # if b is not too large, and a, b, and d are all different
            large = a^2*b^2*d                     # calculate large
            mag = abs(a^2*b^2*d)                  # calculate the magnitude
            M[i,] <- c(a, b, d, q, r, large, mag) # add it to the set
            i <- i + 1                            # move to the next row
          } # /if b is not too large and a, b, and d are all different
        } # /if b is an integer other than -1, 0, or 1
      } # /if b is not complex
    } # /for each value of a
  } # /for each value of r
} # /for each value of q

M2 <- M[M$mag != 0,]

M2$long = -M2$a^2 * M2$b^2 + M2$a * M2$b^2 + M2$a * M2$d # calculate long
M2$medium = M2$a * M2$b^2 + M2$a * M2$d                  # calculate medium
M2$fac1 = M2$a*M2$b^2                                    # calculate fac1
M2$fac2 = M2$a*M2$d                                      # calculate fac2







