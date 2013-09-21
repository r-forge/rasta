### R code from vignette source 'Lesson_1_demo.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Lesson_1_demo.Rnw:89-91
###################################################
a <- 1
a


###################################################
### code chunk number 2: Lesson_1_demo.Rnw:98-107
###################################################
class(a)
b <- 2 
a + b
newfunc <- function(x, y) {
	2*x + y
} 
a2b <- newfunc(2, 4)
a2b
rm(a, b, newfunc, a2b)


###################################################
### code chunk number 3: Lesson_1_demo.Rnw:118-119
###################################################
?class


###################################################
### code chunk number 4: Lesson_1_demo.Rnw:126-131
###################################################
attach(mtcars) ## add it to the search path in R
# you can find more info about the data set via ?mtcars
# the data itself looks like
wt # weigth
mpg # miles per gallon


###################################################
### code chunk number 5: Lesson_1_demo.Rnw:137-142
###################################################
# Creating a Graph
plot(wt, mpg) 
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars) # remove the data from the search path


