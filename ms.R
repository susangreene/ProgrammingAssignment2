# RProgramming
# set the matrix
# getwd()

makecachematrix <- function(zb = matrix()){
  inv = NULL  
  estab = function(c) {  #establish function
    #inv = NULL{          # inverse set to NULL 
    #z <<- c
    zb <<- c
    inv <<- NULL
  }
  
  # get the matrix, set the inverse 
  
  GETinve = function()inv
  list(estab = estab, GETinve = GETinve, GET=GET,  
       estabinve=estabinve)
}

GET = function()zb
estabinve = function(inv2)inv <<- inv2

# retrieve cache info

cachesolve <- function(zb, ...){
  
  inv <- zb$GETinve() 
  if(!is.null(inv)) {
    message("retrieving cached information")  
    
    return(inv)
  }
  
  output <- zb$GET()  # output to be the inverse, solve
  inv <-solve(output)
  zb$estabinv(inv)
  
  inv
}

# perform rbind, set cachematrix

zb = rbind(c(3,1), c(2,3))
rho = makecachematrix(z)
rho$GET()

# run cachesolver to get solution, check solution

cachesolve(rho)    # solution 0.4285  -0.1428
                   #         -0.2857   0.4285

zb = rbind(c(2,4), c(5,3))
rho = makecachematrix(z)
rho$GET()

cachesolve(rho)    # solution -0.214  0.2857
                   #           0.357 -0.1428

zb = rbind(c(10,4), c(9,7))
rho = makecachematrix(z)
rho$GET()

cachesolve(rho)     # solution  0.2058  -0.1176
                    #          -0.2647   0.2941


