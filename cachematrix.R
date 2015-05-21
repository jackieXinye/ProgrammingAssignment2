#~~~~~~~~~~~Write a pair of functions that cache the inverse of a matrix to avoid computing it repeatedly. ~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~This function creates a special "matrix" object that can cache its inverse.~~~~~~~~~~~~~~~~~~~~~~#
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~This function returns the inverse of the matrix. ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#It first checks if the inverse has already been computed. If so, it gets the inverse and skips the computation. 
#If not, it computes the inverse, sets the value in the cache via setInverse function.

#The function assumes that the matrix is invertible. But it checks if the matrix is squared or not. If not, it pops up
#a message that requires entering a new squared matrix and return inverse as NULL.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        if (dim(data)[1]==dim(data)[2]){
                inv <- solve(data)
        }else{
                inv<-NULL
                message("Please enter squared matrix")
        }
        x$setInverse(inv)
        inv
}
