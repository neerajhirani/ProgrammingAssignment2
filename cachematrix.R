## -------------------------------------------------------------------##

## The two functions makeCacheMatrix and cacheSolve below will be 
## used in conjunction to find the inverse of a matrix. If the inverse
## has already been found, it is returned without having to recalculate
## the inverse, thus saving time in computing. 

## -------------------------------------------------------------------##

## Note: The function makeCacheMatrix() has to be run prior to 
## cacheSolve().

## -------------------------------------------------------------------##

## makeCacheMatrix is a function that returns a list of functions. 
## It will store a matrix and a cached value of the inverse of the 
## matrix. It contains the following functions:
## 1. set : Set the value of a matrix.
## 2. get : Get the value of a matrix.
## 3. setInverse : Set the value of the inverse of the matrix.
## 4. getInverse : Get the value of the inverse of the matrix.

## -------------------------------------------------------------------##

makeCacheMatrix <- function(x = matrix()) {
    
    ## Input x will be a matrix.
    
    inv <- NULL
    
    ## inv will be our 'inverse' matrix and it's reset to NULL every
    ## time makeCacheMatrix is called.
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ## Note: These next three functions are defined but not run whenever
    ## makeCacheMatrix is called. Instead, they will be used by cacheSolve()
    ## to get values for x or for inv (inverse) and for setting the inverse.
    
    get <- function () { x }
    
    ## The get function returns the value of the original matrix.
    
    setInverse <- function(inverse) inv <<- inverse
    
    ## The setInverse function is called by cacheSolve() during the first
    ## cacheSolve() access and it will store the value using superassignment.
    
    getInverse <- function() { inv }
    
    ## The getInverse function will return the cached value to cacheSolve()
    ## on subsequent accesses
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
    ## This list is accessed each time makeCacheMatrix() is called, i.e., each 
    ## time we make a new object. THis is a list of the internal functions
    ## (methods), so a calling function knows how to access these methods.
}

## -------------------------------------------------------------------##

## The function cacheSolve() does the actual inversing of the matrix x.
## It first checks if the inverse matrix has been found; if yes, it returns
## the result and quits. If not, the inverse of x is calculated, save to 
## cache, and returned.

## -------------------------------------------------------------------##

## Note: Argument x for this function must be cached, i.e., a list returned
## from calling makeCacheMatrix()

## -------------------------------------------------------------------##

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ## The input x is an object created by makeCacheMatrix
    
    inv <- x$getInverse()
    
    ## Accesses the object 'x' and gets the inverse of the matrix
    
    if(!is.null(inv)){
        
        ## If the inverse was already cached, i.e., not NULL,
        ## send the "message" below to the console and return
        ## the inverse. "return" ends the function cacheSolve().
        
        message("getting cached data.")
        return(inv)
    }
    
    ## We reach the code below only if x$getInverse() returned NULL.
    
    data <- x$get()
    
    ## If inv is NULL, then we have to calculate the inverse.
    
    inv <- solve(data)
    
    ## Now Store the calculated mean value in x (see setInverse() in 
    ## makeCacheMatrix).
    
    x$setInverse(inv)
    
    ## Finally, return the inverse to the code that called the function.
    
    inv
}

## -------------------------------------------------------------------##
