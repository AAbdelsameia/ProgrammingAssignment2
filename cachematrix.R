## the function makeCacheMatrix is used to create a speical matrix that can store its inverse in msol.
## the special matrix is then called by a functoin cacheSolve that checks if the matrix's inverse is already
##calculated and stored in msol. If so, it returns the already saved solution. If not, it calcualtes the inverse, 
## updates the solution in the special msol matrix, and returns the solution. 






## This functoin creates a special matrix that can store its inverse in msol

makeCacheMatrix <- function(m = matrix()) {
  msol <- NULL
  set <- function(y) {
    m <<- y
    msol <<- NULL
  }
  get <- function() m
  setmean <- function(sol) msol <<- sol
  getmean <- function() msol
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This functoin is called with speical matrix as input to check for previos saved inverses,
## if there is non, it calcuates the new inverse, updates msol and prints the solutoin.  

cacheSolve <- function(m, ...) {

  msol <- m$getmean()
  if(!is.null(msol)) {
    message("getting cached data")
    return(msol)
  }
  data <- m$get()
  msol <- solve(data, ...)
  m$setmean(msol)
  msol
        ## Return a matrix that is the inverse of 'x'
}
