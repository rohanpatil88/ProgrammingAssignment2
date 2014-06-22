

## makeCacheMatrix function creates an inverse of given matrix

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x #returns the matrix
        setinv <- function(m) m <<- solve(x) #the matrix is inversed
        getinv <- function() m  #returns the inversed matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve function returns the matrix as it is if already cached else returns inverse of it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <<- x$getinv() 
        if(!is.null(m)) {  #checks if the matrix is cached
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) #inverses the matrix which is not cached
        x$setinv() 
        m
}
