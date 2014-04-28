## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix as an argument and returns a list of functions that can calculate the 
## inverse of a given matrix if the inverse has not been calculated and store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        setInverse <- function(x= matrix()){
                y <- solve(x)
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function gets the inverse of a given matrix if it is not already cached and puts the calculated
## inverse matrix in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
