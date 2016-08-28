## The function makeCacheMatrix returns a list of function
## It can accept a matrix as its parameter
## It has the following functions:
## 1. set(X) , sets/stores the matrix X. It will replace the original matrix
## 2. get() -> returns the matrix, previously set by set(X) function
##    if matrix was not set then it returns NULL
## 3. setinversematrix(X), sets/stores the inverse  
## 4. getinversematrix(), returns the inverse matrix, previously set by setinversematrix(X) function
##    if the inverse matrix was not set then it returns null


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solved) m <<-solved
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function calculates the solved matrix
## It accepts one or more parameters
## The first parameter is function list which has the matrix in it
## ... further arguments passed to or from other methods
## returns: inverse of matrix

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting inverse matrix from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
