


2+2 # addition

5*5 + 2 # multiplication and addition
[1] 27
5/5 - 3 # division and subtraction

log(exp(pi)) # log, exponential, pi

sin(pi/2) # sinusoids

exp(1)^(-2) # power

sqrt(8) # square root

1:5 # sequences


seq(1, 10, by=2) # sequences

rep(2, 3) # repeat 2 three times

#Next, we'll use assignment to make some objects:
  x <- 1 + 2 # put 1 + 2 in object x
x = 1 + 2 # same as above with fewer keystrokes
1 + 2 -> x # same
x # view object x

(y = 9 * 3) # put 9 times 3 in y and view the result

(z = rnorm(5)) # put 5 standard normals into z and print z



x = c(1, 2, 3, 4); y = 2*x; z = c(10, 20); w = c(8, 3, 2)
x * y # 1*2, 2*4, 3*6, 4*8


x + z # 1+10, 2+20, 3+10, 4+20


x + w # what happened here?[1] 9 5 5 12
Warning message:
  In y + w : longer object length is not a multiple of
shorter object length
To work your objects, use the following commands:
  ls() # list all objects


ls(pattern = "my") # list every object that contains "my"


rm(x) # remove object "x"
rm(list=ls()) # remove almost everything (use with caution)
help.start() # html help and documentation
data() # list of available data sets
help(exp) # specific help (?exp is the same)
getwd() # get working directory
setwd() # change working directory



x = 1:3; y = 4:6
(u = c(x, y)) # an R vector
(u1 = cbind(x, y)) # a 3 by 2 matrix


(u2 = rbind(x ,y)) # a 2 by 3 matrix



x = rnorm(25, 10, 4) # generate the data,rnorm is the R function that simulates random variates having a specified normal distribution
c( mean(x), median(x), var(x), sd(x) ) # guess

c( min(x), max(x) ) # smallest and largest values


which.max(x) # index of the max (x[25] in this case)

summary(x) # a five number summary with six numbers

boxplot(x); hist(x); stem(x) # visual summaries (not shown)
