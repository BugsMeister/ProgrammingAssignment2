## There are 2 functions defined here, makeCacheMatrix & cacheSolve
## The purpose of these is to calculate the inverse of a matrix, however rather
## than simply performing the inversion, they cache the result of the inversion.
## So, if the same call is made again, the cached results is returned, rather 
## than having to compute the results again.


###  makeCacheMatrix creates a special "vector", a list containing a function to:
##   set the value of the vector
##   get the value of the vector
##   set the value of the inverse matrix
##   get the value of the inverse matrix
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL              #set <m> in current environment to NULL
  set <- function(y) {   #create a function <set> 
    x <<- y              #which takes the input value <y> and assigns to <x> in parent or global environment
    m <<- NULL           #and set <m> in parent/global envinroment to NULL
  }
  get <- function() {x}         #create function <get> with value of input to <makeCacheMatrix>
  setsolve <- function(solve)   #create a function <setsovle> to the value of the inverse matrix
  m <<- solve                   #set <m> in the parent/global environment as solve
  getsolve <- function() {m}    #create function <getsolve> with value of input to <makeCacheMatrix>
  list(set = set, get = get,    #return a list from the function, 
       setsolve = setsolve,     #with each item in the list set to the 
       getsolve = getsolve)     #object of the same name defined above
}


### cacheSolve calculates the mean of the special "vector" created in makeCacheMatrix
##    First checks to see if the mean has already been calculated. 
##    If so, it gets the mean from the cache and skips the computation. 
##    Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()   #find the value of getsolve from the input object {x}
  if(!is.null(m)) {   #if the value isnt NULL return the value and exit function
    message("getting cached data")
    return(m)
  }
  data <- x$get()        #if the value was NULL, set the object <data> to the value of <get> from the input object {x}
  m <- solve(data, ...)  #apply the function solve() to object <data>
  x$setsolve(m)          #'store' the returned object <m> in <setsolve> in the input object <x>
  m                     #return the object <m>
}
