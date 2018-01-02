#Beastly Rational Equation Problem Creator

#This code will allow you to create problems that result in the form (x^2 - a^2b^2)(x + c)=0, 
#and can be rewritten as  x^3 + (c+1)x + long*x = x^2 + medium*x + large
# or as x(x+q)(x+r) = (x + fac1)(x + fac2)
# which can then be reorganized and unsimplified as a rational equation 


q <- 1
a <- 1
b <- 1
c <- 1
r <- 1

M <- data.frame(0,0,0,0,0,0,9)
M
names(M) <- c("a","b","c","q","r","large", "mag")

i <- 1

M[i,]

for (q in c(-30:30)){
  for (r in c(q:30)){
    c <- q + r - 1
    for (a in c(2:30)){
      if ((q*r-a*c)/(a-a^2)>0){
        b <- sqrt((q*r-a*c)/(a-a^2))
        if((round(b)==b) && (b^2 != abs(b))){
          if ((b < 100000) && (abs(c) != abs(b)) && (abs(b) != abs(a)) && (abs(c) != abs(a))){
            M[i,] <- c(a, b, c, q, r, a^2*b^2*c,abs(a^2*b^2*c))
            i <- i+1
          }
        }  
      }
    }
  }
}
M2 <- M[c(which(M$mag != 0)),]
M2 <- M2[c(which(M2$r != 0)),]
M2 <- M2[c(which(M2$q != 0)),]
M2$long = -M2$a^2 * M2$b^2 + M2$a * M2$b^2 + M2$a * M2$c
M2$medium = M2$a * M2$b^2 + M2$a * M2$c
M2$fac1 = M2$a*M2$b^2
M2$fac2 = M2$a*M2$c
  
which(M2$fac2==0)

str(M2)

grab <- which(M2$mag == min(M2$mag))
M2[grab,]

which(M$q == M$r)


a = M2[min(grab),1]
b = M2[min(grab),2]
c = M2[min(grab),3]
q = M2[min(grab),4]
r = M2[min(grab),5]
long = -a^2*b^2+a*b^2+a*c
medium = a*b^2+a*c
large = a^2*b^2*c
fac1 = a*b^2
fac2 = a*c


write.csv(M2, file = "C:/Docs/GTH/2014-2015/Math dept/stuff.csv")