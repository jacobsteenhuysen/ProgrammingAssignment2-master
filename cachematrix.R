## At first I had a lot of trouble trying to even understand this and
## had to watch a Khan Academy video on what even an inverse matrix was.
## I thought it would just be reversing numbers. 
## Then I followed the "mean example" from the website.

## This function is going to cache a matrix so that you have a matrix to inverse. 
## it is going to:
## Set the matrix.
## Get the matrix.
## Inverse the matrix and set it.
## Then get the inversed matrix.
## Then it's going to put it into a list.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## After you get the inverse cacheSolve is going to calculate the inverse 
## and then cache it. 
## It will:
## Check it to see if it's already been solved.
## If it has, it will respond with: "Retreving Previously Cached Results"
## If it hasn't, it will get the matrix, find the inverse, cache it
## and return the result.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Retreving Previously Cached Results")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
