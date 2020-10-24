## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix which is capable of caching its inverse.

makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(Y){
        X<<-Y
        inv<<-NULL
    }
    get <-function() X
    setinv <- function(new_inv) inv<<-new_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This function calculates the inverse matrix. If it was calculated beforehand, 
##it omits the solving process an returns the value already calculated

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- X$getinv()
    if(!is.null(inv)){ ##If already calculated, returns the inverse
        message("getting cached matrix")
        return(inv)
    }
    data<-X$get() ##gets the data if the inverse was not calculated before
    inv<- solve(data) ##Calculates the inverse
    X$setinv(inv)  ##set the inverse matrix to the CacheMatrix object
    inv ##returns the inverse
}
