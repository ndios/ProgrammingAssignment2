## Functions to calculate matrix inverses and maintain them in cache for efficiency

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
		
        # return list of setter/getter functions for matrices and inverses
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate matrix inverse or get from cache if exists
cacheSolve <- function(x, ...) {
        i <- x$getinverse()  
		
        if (!is.null(i)) {  # inverse in cache?
            message("Getting cached data")
            return(i)  # return inverse
        }
		
        m <- x$get()  
        if(nrow(m)/ncol(m) == 1) {  # square matrix?
            i <- solve(m)  # calculate inverse
            x$setinverse(i)  # save new inverse in cache
        }
        else {
            print("Ooops! Not an square matrix...")
        }
		
        return(i) # return inverse
}
