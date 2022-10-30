# The function c() creates a string of values, 
# and the line below also assigns that string to x:
x <- c(1:5, 15) 
x    # x is now the vector 1 2 3 4 5 15
# Let's find the mean of x. We can use the function mean to do this:       
mean(x)

#### But what if we want to know more about the function mean? For example, 
# we're looking for the arithmetic mean, so we should make sure this function 
# doesn't find the geometric mean.

#### We can use R documentation to see how the function mean() works, by entering
?mean
#### This opens up the R Documentation on the bottom right side of RStudio 
# First, you can look at the Description, which tells you what the function does. 
# This description tells us this function is finding the arithmetic mean. 

#### The usage section tells us how the function is usually used. That first line
# under the usage heading tells us that the argument x is required. 
# This makes sense; you can't use the function if you aren't taking the
# mean of anything. 

#### The next lines tell you the default arguments included in this function
# are x, trim = 0, and na.rm= FALSE.This means th






+-------+
  
  "}at if you use mean without 
# the arguments trim and na.rm, they will automatically be set to 0 and FALSE,
# respectively. 

##### So, if we include these defaults in our command, we'll get the same answer 
# we got when we didn't include them. 
mean(x = x, trim = 0, na.rm = FALSE)

#### The arguments included in a function are explained in more detail 
# under the Arguments heading. 

#### If you want to see examples of how a function works, you can use the
# example function: 
example(mean)

#### In this case, the example is showing how the trim argument works, by 
# showing that you get a different average when trim = 0.10
# Note that doing this example has changed the value of x in our workspace!
