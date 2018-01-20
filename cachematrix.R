## Put comments here that give an overall description of what your
## functions do

## These two function together can calculate the inverse of a matrix in
## a cache stored way. The makeCacheMatrix can store the inverse of a matrix 
## in a specific environment and when the inverse is needed in the future, it
## can get the inverse directly instead of calculating it again. 
## Assume the target matrix is Mat, to apply these two functions, the commands
##in main function should look like this:
##
## cacheMat <- makeCacheMatrix(Mat)
## inv <- cacheSolve(cacheMat)
## inv <- cacheSolve(cacheMat)
## ...
##

## Write a short comment describing this function
## This function creates a list of function bonding to the original matrix
## set function makes the initialization and store it in a specific environment
## get function returns the matrix
## setinverse function stores the inverse of original matrix in the specific environment mentioned above
## getinverse function return sthe inverse of original matrix from the specific environment
## 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## This function solve the inverse of a matrix in a cache stored manner.
## It first query whether there has already been an inverse in a specific environment
## If yes, it will read the inverse and return it togther, otherwise it will solve the 
## inverse and store it in the environment
## So generally speaking it only needs to solve the inverse the firest time it needs it,
## and can read the inverse directly in the following iterations.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
