## The following two functions are used to cache the inverse of a matrix
## This is useful since it is a costly operation

## This function creates a list containing functions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the matrix
## It first tries to get the data from the cache
## If this data does not exist, then it calculates the inverse matrix and sets it in the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
