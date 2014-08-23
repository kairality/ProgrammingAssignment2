## The calculation of the matrix inverse is costly, so it's beneficial to
## store the value and re-use it later as long as the values of our matrix
## have not been changed.  Function environments are destroyed after they
## finish their work though, so we have to use '<<-" (the superassignment
## operator) to get the job done.  The first function "makeCacheMatrix"
## approximates the creation of a class in other languages, creating one
## instance of a "cache matrix class" without formal definition.  When
## we call "cacheSolve" on a cache matrix, we do not re-calculate the inverse
## if the inverse has already been calculated and the cache matrix has not
## been altered via the "set" method.  This program is based on rdpeng's 
## Cache vector script.

## Create a single "cache matrix", returns a list of functions that define
## our matrix object.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse) {
                inv <<- inverse
        }
        getinv <- function() {
                inv
        }
        return (list(set = set, get = get, setinv = setinv, 
                     getinv = getinv))

}


## Calculate the inverse of a cache matrix if it has not yet been solved.
## If the inverse has already been solved and the values have not changed,
## we retrieve cached data.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)  # set the inverse so we retrieve cached data later
        return(inv)
        
}
