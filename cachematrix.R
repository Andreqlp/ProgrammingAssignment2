## Put comments here that give an overall description of what your
## In makeCacheMatrix function, a link is created with the variables "x" and "m" for information that is outside the creator of the function room. Moreover inverse functions are created for display based on the variable "m" and a function generating a new matrix (getInverso).

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

##The cacheSolve function assigns a result of the inverted matrix function for the variable 'm', provided that it be a null value.

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
