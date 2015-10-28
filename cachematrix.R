## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverso <- function(inverso) m <<- inverso
        getInverso <- function() m
        list(set = set, get = get,
             setInverso = setInverso,
             getInverso = getInverso)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverso()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        dados <- x$get()
        m <- solve(dados, ...)
        x$setInverso(m)
        m
}
