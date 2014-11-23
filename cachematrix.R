## This function will create an object (of type matrix) which has the capability to
## cache the inverse of itself and return it everytime its inverse is requested


makeCacheMatrix <- function(cacheMatrix = matrix()) {  # input cacheMatrix will be a matrix
    # invMatrix will be the inverse matrix and is reset to NULL everytime 
    # makeCacheMatrix is called
    invMatrix <- NULL        
    # This function takes an input matrix and 
    # assigns it to the cacheMatrix
    setCacheMatrix <- function(inputMatrix) {
        cacheMatrix <<- inputMatrix
        invMatrix <<- NULL
    }
    # This function returns the original cacheMatrix
    getCacheMatrix <- function() cacheMatrix
    # This is used to assign the inverse matrix of cacheMatrix
    # This is called by first cacheSolve() and will store it 
    # for subsequent access
    setInverse <- function(inverseMatrixInput) invMatrix <<- inverseMatrixInput
    # This is used to get the inverse and called by cacheSolve on subsequent accesses
    getInverse <- function() invMatrix
    
    list(set = setCacheMatrix, get = getCacheMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(inputMatrix, ...) { # The input inputMatrix is a cacheMatrix created by makeCacheMatrix
    # It first gets the inverse matric
    invMatrix <- inputMatrix$getInverse()
    # If the inverse matrix is already present
    if(!is.null(invMatrix)) {
        # Print the message to console
        message("getting inverse matrix")
        # and return the inverse matrix
        return(invMatrix)
    }
    # This is only when inverse was not calculated
    data <- inputMatrix$get()
    # Calculate the inverse of the inputMatric and assign
    invMatrix <- solve(data)
    # Save for future accesses
    inputMatrix$setInverse(invMatrix)
    # Return the inverse matrix
    invMatrix
}
