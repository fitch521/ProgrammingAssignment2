## This function returns the inverse of the matrix. If the inverse
## has already been calculated(and the matrix has not changes), then
## the cacheSolve would retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        S_inverse <- function(inverse) inv <<- inverse
        G_inverse <- function() inv
        list(set=set, get=get, S_inverse=S_inverse, G_inverse=G_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x%G_inverse()
        if(!is.null(inv)){
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$S_inverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
