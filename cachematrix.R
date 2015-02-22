## cacheMatrix.R 
## Description: contains a pair of function that allows caching of the inverse
## of a matrix to save computational time
##
## Ex:
## > matrixA <- matrix(c(2,2,3,2), nrow = 2, ncol = 2)
## > matrixACache <- makeCacheMatrix(matrixA)
## > cacheSolve(matrixACache)
##     [,1] [,2]
## [1,]   -1  1.5    ====>    inverse matrix of matrixA now stored
## [2,]    1 -1.0             in matrixACache 



## makeCacheMatrix returns a list containing functions to set and get 
## the input matrix as well as set and get functions for the inverse of said matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## stores cached inverse of matrix
    inverse <- NULL
    
    ## function to set input matrix value
    set <- function(y){
        
        x <<- y
        inverse <<- NULL
    }
    
    ## get input matrix value
    get <- function() x
    
    ## set the cached inverse
    setInverse <- function(newInverse) inverse <-- newInverse
    
    ## get the cached inverse
    getInverse <- function()inverse
    
    ## return a list of the above functions
    return(list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve calculates the inverse of the matrix and stores 
## it in special list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        ## if the inverse already exists, return the inverse
        if(!is.null(inverse)){
            message("getting cached data...")
           return(inverse)
        }
        
        ## if inverse doesn't exist, get matrix from special list
        dataMatrix <- x$get()
        
        ## calculate inverse of matrix (we assume matrix is invertible)
        inverse <- solve(dataMatrix)
        
        ## set our special list with the newly-found inverse
        x$setInverse(inverse)
        
        #return inverse
        return(inverse)
}
