makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix
        ## return: a list containing functions to

        inv <-NULL
        set <-function(y) {
                x <<- y
                inverse <<- NULL
                # <<- to assign a value to an object 
        }
        get <- function() x
       ## set the inverse
         setinv <- function(inverse) inv <<- inverse
        ## get the inverse
        getinv <- function() inv
        ## list is used as the input to cacheSolve()
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <-function(x, ...) {
        ## x: output of makeCacheMatrix()
        
        inv <- x$getinv()
        
        ## return: inverse of the original matrix input to makeCacheMatrix()
        # else calculate the inverse 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix.data <- x$get()
        inv <- solve(matrix.data,...)
        x$setinv(inv)
        return(inv)
}