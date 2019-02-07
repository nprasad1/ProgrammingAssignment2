## Put comments here that give an overall description of what your
## functions do

# Creates a special matrix that is actually a list (?) containing functions to 1) set and 2) get the value
#of the matrix, and 3)set and 4)get the value of the inverse of the matrix.  
makeCacheMatrix <- function(x = matrix()) {     #takes a matrix x, and defaults x to an empty matrix. 
        i <- NULL               #assigns i in this function as NULL
        set <- function(y) {    
                x <<- y         #set is a  function that takes input matrix y and assigns it to "x" in your workspace
                i <<- NULL      #..and makes sure i in your workspace is NULL (it may or may not have already been).
        }
        get <- function() x     #get() returns x....confused
        setinverse <- function(inverse) i <<- inverse   #setinverse assigns its input to i in your workspace
        getinverse <- function() i    #Returns i (from this environment?)
        list(set = set, get = get,
             setinverse = setinverse,   #These functions are all stored as a list, and this function returns that list, 
             getinverse = getinverse)   #so x and i will be returned and autoprinted
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#and sets the value of the inverse matrix in the cache via the setinverse function.  
#However, it first checks to see if the inverse has already been calculated (and the matrix has not changed).
#If it has already been calculated, then cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinverse()    #look up the getinverse() object in the list and if it has anything in it, return it. 
        if(!is.null(i)) {
                print("getting cached data")
                return(i)
        }
        data <- x$get()     #if there's nothing in getinverse() yet, then gets the matrix from the get
        i <- solve(data, ...)  #...object in the list and calculates its inverse, sets it into set_inverse, 
        x$setinverse(i)    #...returns it, and now get_inverse is also populated.  
        i
}