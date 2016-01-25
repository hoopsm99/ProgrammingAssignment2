## -->> These two functions together compute the inverse of a matrix 
## and cache it for later use because computing the inverse of a matrix
## can be a costly computation.  

## Use: cacheSolve(makeCacheMatrix(x)) where x is an invertible matrix

## -->> makeCacheMatrix() 
## This function creates a special "matrix" object that 
## can cache its inverse; creates list of functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }  
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, 
           get=get, 
           setinverse=setinverse, 
           getinverse=getinverse)
}

## -->> cacheSolve() 
## returns the inverse of the matrix.  
## First it checks to see if the inverse has already been cached. If so it sets
## the inverse previously computed. If not, it uses the solve() function to 
## return the inverse of the matrix.
## Note: assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
