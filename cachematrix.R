## makeCacheMatrix and cacheSolve are methods that allows caching of a matrix inverse
## Usage: makeCacheMatrix([existing matrix]) returns a CachableMatrix
## Instead of calling solve(matrix), call cacheSolve(matrix), and you're done!

## Returns a Matrix that allows computation of its inverse and caches it

makeCacheMatrix <- function(mat = matrix()) {
    # inverse is NULL when we start off
    inverse <- NULL
    
    # function that sets the matrix, and sets inverse to NULL
    set <- function(y) {
      mat <<- y
      inverse <<- NULL
    }
    # function that gets the matrix
    get <- function() {
      return(mat)
    }
    # function that sets the inverse of the matrix
    setinverse <- function(inv) {
      inverse <<- inv
    }
    # function that gets the inverse of the matrix
    getinverse <- function() {
      # imo, cacheSolve should be called here. More OOP-y
      return(inverse)
    }
    
    # construct the list object and return it  
    return(list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse))
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
  
    # first, check if inverse has been computed, if no, compute
    if(is.null(mat$getinverse())) { # inverse has not been computed

      # get the inverse
      inverse <- solve(mat$get()) 
      # cache it
      mat$setinverse(inverse)
    } else {
      message("getting cached data")
    }
    
    return(mat$getinverse())
}


##### Tests ##### 
# comment out the message("getting cached data") before running this test

# create a 10x10 matrix
mat <- makeCacheMatrix(matrix(rnorm(100), c(10,10)))
stopifnot(!is.null(mat$get()))
print("get() Works")
mat$set(matrix(rnorm(10000), c(100,100)))
stopifnot(dim(mat$get()) == c(100,100))
print("set() works")
stopifnot(is.null(mat$getinverse()))
print("inverse is correctly null")
# calculating and caching inverse
cacheSolve(mat)
stopifnot(!is.null(mat$getinverse()))
print("cachesolve seems to work")
print("If the next print occurs immediately, then caching is working")
for(i in 1:10000) {
  cacheSolve(mat)
}
stopifnot(!is.null(mat$getinverse()))
print("Called cacheSolve() 10,000 times")
