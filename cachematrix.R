## Put comments here that give an overall description of what your
## functions do

## This function does essentially the same thing as in the example
## Rather than dealing with a one-element numeric vector, and its
## one-element numeric mean, it takes a matrix and its matrix inverse
## makeCacheMatrix returns a list of functions, similar to the example
## SetMat is used to change the matrix, nullifying the now-incorrect inverse
## GetMat returns the most recently SetMat-rix
## SetInv will overwrite the cached inverse with a new matrix
## getInv returns the cached value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    
    setMat <- function(y) {
      x <<- y
      xInv <<- NULL
    }
    
    getMat <- function() x
    
    setInv <- function(Inv) xInv <<- Inv
    
    getInv <- function() xInv
      
    list(setMat = setMat, getMat = getMat,
           setInv = setInv,
           getInv = getInv)
}


## This function checks first to see if a matrix has been cached
## Does NOT check to see if the inverse is correct first, though
## and if one is saved, returns it, if not, the value is
## calculated, cached for later, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInv()

    if (!is.null(Inv)){return(Inv)}
    
    xInv = solve(x$getMat())
    x$setInv(Inv)
    xInv
}
