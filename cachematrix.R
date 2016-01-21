## These two functions would achieve the goal of calculating the inverse of a 
##given matrix (given that this matrix is inversible), and save the inverse so 
##that the user can just pull out the saved inverse of a known matrix instead of
##do the calculation again (which can be time consumimg). 

## The first function "makeCacheMatrix" creates a list of funcions and an object
##"m" to cache the inverse of the given matrix.  As "m" is cached in the parent 
##environment, user can access the inverse saved in m to avoid repeating 
##calculation. Input to the function should be a inversible matrix.

makeCacheMatrix <-function (x=matrix()) {
        m <-NULL		
        set <- function(y) {
                x <<- y
                m <<- NULL		
        }
        get <- function () x
        setinverse <- function(inverse) m <<- inverse		
        getinverse <- function () m
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)		
}

## The second function "cacheSolve" would give the user the inverse of a matrix.
##It would first determine whether there's a cached inverse that it can just 
##output for the given matrix. If not, it will calculate the inverse and cache it
##in object "m" by calling upon functions in the environment of "make 
##CacheMatrix". Input to this function should be the list of functions returned 
##by the function "makeCacheMatrix". 

cacheSolve <- function(x,...) {
        m <- x$getinverse()
        if (!is.null(m)) {		
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m<- solve(matrix)
        x$setinverse(m)
        m
}
