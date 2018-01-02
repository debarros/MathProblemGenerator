#Beastly Logarithm Problem Creator

#This program generates problems of the form [log {x+r} (ax+b)]^2 - [log {x+r} (ax+b)^5] + 6 = 0.
#Note that {x+r} is the base of the logs.
#This quadratic expression factors into [log{x+r}(ax+b) - 2][log{x+r (ax+b) - 3]=0
#The first factor results in a simple quadratic trinomial.
#The second factor results in a cubic that can be solved using factor-by-grouping
#The process always generates 5 solutions, but (so far) each problem has only 2 valid solutions

#Initialize variables
i = 0
r = 2
a = 2
b = 2
rvec = numeric(0)
avec = numeric(0)
bvec = numeric(0)
Z4vec = numeric(0)
Z3vec = numeric(0)
Z1vec = numeric(0)
Z2vec = numeric(0)

#Start by generating  r, a, and b
for (r in -50:50){
  for (a in -50:50){
    for (b in -50:50){
      if ((a > 3*r^2) && r!=0 && a != 0 && b!= 0 && ((2*r-a)^2 > 4*(r^2-b))){   #Check to make sure no imaginary number appear, and that no parameter == 0
        i = i + 1  
        rvec[i] = r
        avec[i] = a
        bvec[i] = b
        Z4vec[i] = -3*r                                       #This is the root from the linear part of the factor-by-grouping
        Z3vec[i] = sqrt(a-(3*(r^2)))                          #This is the positive root from the difference of squares in the factor by grouping
        Z1vec[i] = (a - 2*r + sqrt((2*r-a)^2 - 4*(r^2-b)))/2  #This is one of the roots from the quadratic trinomial
        Z2vec[i] = (a - 2*r - sqrt((2*r-a)^2 - 4*(r^2-b)))/2  #This is the other root from the quadratic trinomial
      }
    }
  }
}



#Compile everything into a data frame
M = data.frame(rvec, avec, bvec, Z1vec, Z2vec, Z3vec, Z4vec)

#Eliminate the cases in which the the solutions are not integers
M1 = M[which(M$Z1vec == round(M$Z1vec)),]
M2 = M1[which(M1$Z2vec == round(M1$Z2vec)),]
M3 = M2[which(M2$Z3vec == round(M2$Z3vec)),]
M4 = M3[which(M3$Z4vec == round(M3$Z4vec)),]

#Eliminate the cases in which any solution is 0 (this step may be unnecessary)
M5 = M4[which(M4$Z1vec !=0),]
M6 = M5[which(M5$Z2vec !=0),]
M7 = M6[which(M6$Z3vec !=0),]
M8 = M7[which(M7$Z4vec !=0),]

#Check to make sure the positive root from the difference of squares in the factor-by-grouping part actually works
M9 = M8[which((M8$Z3vec + M8$rvec)^3 == M8$avec * M8$Z3vec + M8$bvec),]

#Add in the negative root from the difference of squares in the factor-by-grouping part
M9$Z5vec = M9$Z3vec * -1

#Export the results
write.csv(M9, file = "C:/Docs/GTH/2014-2015/Math dept/stuff2.csv")
