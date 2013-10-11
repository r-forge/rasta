### R code from vignette source 'pkg/rasta/vignettes/Lesson_1_demo.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: Lesson_1_demo.Rnw:80-82
###################################################
a <- 1
a


###################################################
### code chunk number 2: Lesson_1_demo.Rnw:89-98
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
### code chunk number 3: Lesson_1_demo.Rnw:109-110
###################################################
?class


###################################################
### code chunk number 4: Lesson_1_demo.Rnw:117-122
###################################################
attach(mtcars) ## add it to the search path in R
# you can find more info about the data set via ?mtcars
# the data itself looks like
wt # weigth
mpg # miles per gallon


###################################################
### code chunk number 5: Lesson_1_demo.Rnw:128-133
###################################################
# Creating a Graph
plot(wt, mpg) 
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars) # remove the data from the search path


