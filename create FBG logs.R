# FBG Log Problem Generator
# by Paul de Barros


# This program creates problems of the form L^3 - L^2 - 4L + 4 = 0, 
#   where L is log {ax+b} (cx+d), for integers a,b,c,d,
#   where {ax+b} is the base of the log and (cx+d) is the argument.
# Factor by grouping allows 3 separate equations to come from this,
#   leading to 1 linear, 1 quadratic, and 1 cubic amenable to factor-by-grouping.

gc()
library(gtools)    # this is needed for the permutation function
library(rmarkdown) # This is needed to produce output with subscripts and superscripts

# set the max absolute value of the generated parameters
n = 40 


# Create all sets of parameters to be used ####
# data.frame x holds sets of the 4 parameters (a, b, c, and d)
x = permutations(n = 2*n, r = 4, v = c(-1*(1:n),1:n), repeats.allowed = TRUE)  
gc()
xa = x[,1]
xb = x[,2]
xc = x[,3]
xd = x[,4]

# Remove entries where the coefficients in a linear thingy are the same ####
x = x[xa != xb & xc != xd,]

xa = x[,1]
xb = x[,2]
xc = x[,3]
xd = x[,4]
neg.q2.r = (xc * (xb^2)) + 2*xa*xb*xd
gc()

# Remove entries where there would be a missing term in the cubic equation ####
x = x[neg.q2.r != 0,]
gc()

xa = x[,1]
xb = x[,2]
xc = x[,3]
xd = x[,4]
sr1 = (xd*xa + (2*xb*xc)) / (xc * xa)
neg.q2.r = (xc * (xb^2)) + 2*xa*xb*xd
gc()
sr2 = ((xd * (xb^2)) - 1) / neg.q2.r
gc()

# Remove entries that would not produce a cubic equation ####
x = x[sr1 == sr2,] 

gc()
xa = x[,1]
xb = x[,2]
xc = x[,3]
xd = x[,4]
sr1 = (xd*xa + (2*xb*xc)) / (xc * xa)
neg.q2.r = (xc * (xb^2)) + 2*xa*xb*xd
sr2 = ((xd * (xb^2)) - 1) / neg.q2.r
radicand = (2*xa*xb - xc)^2 - 4*(xa^2)*(xb^2 - xd)
root = sqrt(radicand)
rount = round(root)

# Remove entries that would produce an unfactorable quadratic ####
x = x[root == rount,]

xa = x[,1]
xb = x[,2]
xc = x[,3]
xd = x[,4]

# Calculate the parameters to be used in the cubic formula ####
# The variables ca, cb, cc, and cd represent the coefficents in a cubic equation
ca = as.complex(xa*xc)
cb = as.complex(xa*xd + 2*xa*xb*xc)
cc = as.complex(2*xa*xb*xd + xc*xb^2)
cd = as.complex(xd*xb^2 - 1)
cub1 = (-1*cb^3)/(27*ca^3) + (cb*cc)/(6*ca^2) - (cd)/(2*ca)
cub2 = (cc)/(3*ca) - (cb^2)/(9*ca^2)
radicand2 = as.complex((cub1)^2 + (cub2)^3)
root2 = sqrt(radicand2)
solution = (cub1 + root2)^(1/3) + (cub1 - root2)^(1/3) - cb/(3*ca)

# Remove entries that result in cubic equations with complex solutions ####
x = x[Im(solution) == 0,]


print(x)
