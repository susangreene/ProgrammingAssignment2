## This sets a matrix, uses a cache function within 
## the matrix, and gets the matrix in order 
## so that the inverse can be calculated.
## Cachesolve is then used to calculate the solution to the inverse.


# set the matrix

makecachematrix <- function(z = matrix()) {
  
  estab = function(c)  #establish function
  inv = NULL{          # inverse set to NULL 
    inv <<- NULL
    z <<- c
  }
  
  # get the matrix, set the inverse 
  
  GETinve = function()inv
  list(estab=estab, GETinve = GETinve, GET=GET,  
       estabinve=estabinve)
}
  
  GET = function() z
  estabinve = function(inv2) inv <<- inv2
  
  # retrieve cache info

cachesolve <- function(z, ...){
  
  inv <- z$GETinve() 
   if(!is.null(inv)) {
    message("retrieving cached information")  
    return(inv)
  }
  
  output <- z$GET()  # output to be the inverse, solve
  inv <-solve(output)
  z$estabinv(inv)
  inv
}

# perform rbind, set cachematrix

z = rbind (c(3,1), c(2,3))
r = makecachematrix(z)
r$GET()

# run cachesolve(r) to get solution

cachesolve(r)    # solution 0.4285  -0.1428
                 #         -0.2857   0.4285

z = rbind(c(2,4), c(5,3))
r = makecachematrix(z)
r$GET()

cachesolve(r)    # solution -0.214  0.2857
                 #           0.357 -0.1428

z = rbind(c(10,4), c(9,7))
r = makecachematrix(z)
r$GET()

cachesolve(r)     # solution  0.2058  -0.1176
                  #          -0.2647   0.2941






