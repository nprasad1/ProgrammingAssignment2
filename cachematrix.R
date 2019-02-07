# These two functions allow the user to input a matrix, calculate its
# inverse, and automatically save the inverse value to cache, so that R can pull
# the inverse value from cache in the future instead of recomputing it.  

# Creates a list containing functions to 1) set and 2) get the value of a 
# matrix, as well as 3) set and 4) get the value of the corresponding inverse 
# matrix from the cache.    
makeCacheMatrix <- function(x = matrix()) {     
        i <- NULL               
        set <- function(y) {    
                x <<- y         
                i <<- NULL      
        }
        get <- function() x     
        setinverse <- function(inverse) i <<- inverse   
        getinverse <- function() i    
        list(set = set, get = get,
             setinverse = setinverse,    
             getinverse = getinverse)   
}


#The function below calculates the inverse of the matrix created by using
#makeCacheMatrix(), returns it, and saves it in cache via setinverse(), 
#if an inverse has not already been calculated for that matrix.  If it has been
#calculated, it simply returns the saved inverse matrix from cache memory.  
cacheSolve <- function(x, ...) {
        i <- x$getinverse()    
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()     
        i <- solve(data, ...)   
        x$setinverse(i)      
        i
}