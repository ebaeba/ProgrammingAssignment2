## the two functions makeCachMatrix and cacheSolve make it possible to cache the value
## of the inverse of a given matrix. Thus this value has only to be calculated once per session
##
## makeCacheMatrix takes a matrix as an argument and generates some useful objects around it in 
## order to be able to cache the inverse of x
##
## cacheSolve takes a "cached" matrix generates by makeCacheMatrix as an argument and returns the
## inverse of this matrix



## take a matrix x an add the following 5 objects:
## 1. a variable inv that shall contain the inverse of x
## 2. a set function in order to be able to set the value of x
## 3. a get function in order to be able to get the value of x
## 4. a setinverse function in order to be able to set the value of the inverse of x
## 5. a getinverse function in order to be able to get the value of teh inverse of x

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculate the inverse of a matrix x using a caching mechanism for the result
## thus, during one session, the calculation of the inverse is done only once (or not at all)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)

        return(inv)
}

## test it for a matrix with known solution
##
## 1    2    0     inverse        -3    2    0
## 2    3    0      -->>           2   -1    0
## 3    4    1                     1   -2    1

tcs <- function() {
        x <- matrix(c(1,2,3,2,3,4,0,0,1), nrow = 3, ncol = 3)
        print(x)
        z <- makeCacheMatrix(x)
        print(cacheSolve(z))
        print(cacheSolve(z))              
}