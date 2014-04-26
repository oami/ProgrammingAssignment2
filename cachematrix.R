## A pair of functions below may be used for caching the inverse of matrix
## rather than computing it on every call

## This function creates a special "matrix" object which consists of
## matrix itself, the cached inverse of the matrix, and internal functions
## used to get or set internal data

makeCacheMatrix <- function(x = matrix()) {
            ## Initial inverse is not computed
            invx <- NULL
            ## If the data of the initial matrix has changed, the inverse is set to NULL
            set <- function(y){
                    x <<- y
                    invx <<- NULL
            }
            get <- function() x
            setinverse <- function(inv) invx <<- inv
            getinverse <- function() invx
            ## Result is returned as a list containing set/get functions
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## This function computes the inverse of "matrix" created with the previous function
## If the inverse has already been calculated, the cached inverse is returned
## Otherwise the function performs a call to solve() and returns its result

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)){
        ## the inverse has already been calculated
                message("getting cached data")
        ## Return cached inverse of 'x'
                return(invx)
        }
        ## Getting matrix and perform the inverse of the matrix
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        ## Return a matrix that is the inverse of 'x'
        invx
}
