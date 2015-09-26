## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function takes a matrix and converts it into a list of 
## set, get, setInverse and, getInverse routines
## 
makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        ## set the Matrix as a globalenv variable
        ## create a new globalenv variable matrixInverse to NULL
        ## if it already exists, clear it.
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
    
        }
        ## return the matrix
        get <-function() x
        
        ## store the Inverse of the matrix in variable matrixInverse
        setinverse <- function(inverseMatrix) matrixInverse <<- inverseMatrix
        
        ## retrieve the inverse of the matrix
        getinverse <- function() matrixInverse
        
        ##Return the list
        list(set=set, get= get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        if(nrow(x$get()) != ncol(x$get())) {
                message("Can't compute inverse on non-square matrix")
        }
        else {
                m <-x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data)
                x$setinverse(m)
                m
        }
}
