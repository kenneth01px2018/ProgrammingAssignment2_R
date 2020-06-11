## Put comments here that give an overall description of what your
## functions do

# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # set x to y
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get x
    get <- function() x
    # set i to inverse
    setinverse <- function(inverse) i <<- inverse 
    # return the inverse
    getinverse <- function() i
    # return list output
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


# computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    # if inverse already solved, return inverse with message
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # solve for inverse
    data <- x$get()
    i <- solve(data) %*% data
    x$setinverse(i)
    i
}
