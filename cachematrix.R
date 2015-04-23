## makeCacheMatrix pulls in a matrix, x
## First it sets the value of the matrix
## then it gets the value of the matrix
## then does the same for the value of the inverse

makeCacheMatrix <- function(x = matrix()){
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     get <- function() x
     setInverse <- function(inverse) i <<- inverse
     getInverse <- function() i
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve first checks if the inverse has already been set
## if not, it'll print a message
## then it calls the solve function to calculate the inverse
## and passes it on to set the value of the inverse in the cache

cacheSolve <- function(x, ...){
     i <- x$getInverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setInverse(i)
     i
}