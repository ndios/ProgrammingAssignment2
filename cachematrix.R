## Functions to calculate matrix inverses and maintain them on cache for efficiency

## Create, maintain and manage a matrix cache
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) i <<- mean
        getinverse <- function() i
		
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate matrix inverse or get from cache if exists
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
		if (!is.null(i)) {
			message("Getting cached data")
			return(i)
		}
		
		m <- x$get()
        
        # square matrix?
		if(nrow(m)/ncol(m) == 1) {
            i <- solve(m)
            x$setinverse(i)
		}
		else {
			print("Hey! The matrix is supposed to be square...")
		}
		
        i
}
