rm(list=ls()) # Clear out workspace


# Part of a tutorial taken from 
# http://adv-r.had.co.nz/Functions.html
# Starting from the Lexical Scoping > Name Masking section.



# The following example illustrates the most basic principle of 
#   lexical scoping, and you should have no problem predicting the 
#   output.
f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)



# If a name isn’t defined inside a function, R will look one level up.
x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
g()
rm(x, g)



# The same rules apply if a function is defined inside another 
#   function: look inside the current function, then where that 
#   function was defined, and so on, all the way up to the global 
#   environment, and then on to other loaded packages. Run the 
#   following code in your head, then confirm the output by running 
#   the R code.
x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()
rm(x, h)



# The same rules apply to closures, functions created by other 
#   functions. Closures will be described in more detail in 
#   functional programming; here we’ll just look at how they interact
#   with scoping. The following function, j(), returns a function. 
#   What do you think this function will return when we call it?
j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()
rm(j, k)



# This seems a little magical (how does R know what the value of y is
#   after the function has been called). It works because k preserves 
#   the environment in which it was defined and because the 
#   environment includes the value of y. Environments gives some 
#   pointers on how you can dive in and figure out what values are 
#   stored in the environment associated with each function.


### Functions vs. variables

# The same principles apply regardless of the type of associated 
#   value - finding functions works exactly the same way as finding 
#   variables:

l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  l(10)
}
m()
#> [1] 20
rm(l, m)



# For functions, there is one small tweak to the rule. If you are 
#   using a name in a context where it’s obvious that you want a 
#   function (e.g. f(3)), R will ignore objects that are not functions
#   while it is searching. In the following example n takes on a 
#   different value depending on whether R is looking for a function 
#   or a variable.
n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()
#> [1] 5
rm(n, o)



# However, using the same name for functions and other objects will 
#   make for confusing code, and is generally best avoided.

### A fresh start

# What happens to the values in between invocations of a function? 
#   What will happen the first time you run this function? What will 
#   happen the second time? (If you haven’t seen exists before: it 
#   returns TRUE if there’s a variable of that name, otherwise it 
#   returns FALSE.)
j <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  print(a)
}
j()
rm(j)



# You might be surprised that it returns the same value, 1, every 
#   time. This is because every time a function is called, a new 
#   environment is created to host execution. A function has no way to
#   tell what happened the last time it was run; each invocation is 
#   completely independent. (We’ll see some ways to get around this in
#   mutable state.)



### Dynamic lookup



# Lexical scoping determines where to look for values, not when to 
#   look for them. R looks for values when the function is run, not 
#   when it’s created. This means that the output of a function can be
#   different depending on objects outside its environment: 
f <- function() x
x <- 15
f()
#> [1] 15

x <- 20
f()
#> [1] 20



# You generally want to avoid this behaviour because it means the 
#   function is no longer self-contained. This is a common error - if 
#   you make a spelling mistake in your code, you won’t get an error 
#   when you create the function, and you might not even get one when
#   you run the function, depending on what variables are defined in 
#   the global environment.

# One way to detect this problem is the findGlobals() function from 
#   codetools. This function lists all the external dependencies of a
#   function:
  
f <- function() x + 1
codetools::findGlobals(f)
#> [1] "+" "x"

# Another way to try and solve the problem would be to manually 
#   change the environment of the function to the emptyenv(), an 
#   environment which contains absolutely nothing:
environment(f) <- emptyenv()
f()
#> Error: could not find function "+"



# This doesn’t work because R relies on lexical scoping to find 
#   everything, even the + operator. It’s never possible to make a 
#   function completely self-contained because you must always rely on
#   functions defined in base R or other packages.

# You can use this same idea to do other things that are extremely
#   ill-advised. For example, since all of the standard operators in R
#   are functions, you can override them with your own alternatives. 
#   If you ever are feeling particularly evil, run the following code 
#   while your friend is away from their computer:
  
# `(` <- function(e1) {
#   if (is.numeric(e1) && runif(1) < 0.1) {
#     e1 + 1
#   } else {
#     e1
#   }
# }
# replicate(50, (1 + 2))
# #>  [1] 3 3 3 3 3 3 3 3 3 3 4 3 3 3 3 3 3 3 3 3 3 4 3 3 3 3 3 4 3 3 3 3 3 3 3
# #> [36] 3 3 3 3 3 3 3 3 4 3 4 3 3 3 3
# rm("(")

# This will introduce a particularly pernicious bug: 10% of the time,
#   1 will be added to any numeric calculation inside parentheses. 
#   This is another good reason to regularly restart with a clean R 
#   session!

rm(list=ls())


### Every operation is a function call

# To understand computations in R, two slogans are helpful:
  
#  Everything that exists is an object.
#  Everything that happens is a function call.
#                          — John Chambers

# There was more to this, and I read it, but I'll do the cut and paste
#   some other time.