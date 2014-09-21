## Put comments here that give an overall description of what your
## functions do:
## makeCacheMatric: takes a matrix and create a cachable matrix object
## cacheSolve: computes the inverse of a square matrix.

## Write a short comment describing this function
## makeCacheMatrix: take a matrix and create a cachable matrix object that contatins
## the following functions:
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the mean
## 4) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    # intialize the inverse matrix  to NULL
    s <- NULL
    
    # set the input matrix x 
    set <- function(y) {
        x <- y
        s <- NULL
    }
    
    # returns the matrix
    get <- function() x
    
    # set the inverse matrix
    setSolve <- function(solve) s <<- solve
    
    #return the inverse matrix
    getSolve <- function() s
    
    list(set = set, 
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function
## cacheSolve: take a makeCacheMatrix object and returns the cached value the inverse matrix if exits.
##              if a cached value doesn't exits, it calculates the inverse matrix and cache it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the inverse matrix 
    s <- x$getSolve()
    
    # if inverse matrix exists in cache, return it
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    
    #compute the inverse matrix 
    s <- solve(data, ...)
    
    #save the computed inverse matrix in cache
    x$setSolve(s)
    s
}
