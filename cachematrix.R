## This function defines sub functions for use in the casheSolve

makeCacheMatrix <- function(x = matrix()) {
	##cleans the cashed variable
       i <- NULL
	## sets the active matrix to a defined value and clears the cashe
      set <- function(y) {
                x <<- y
                i <<- NULL
        }
	##
      get <- function() x
	## sets the cahsed value to the input
      setinv <- function(invmat) i <<- invmat
 	##provides the cashed
	getinv <- function() i
	##enables easy calling of above functions
      list(set = set, get = get, setinv = setinv, getinv = getinv)}


## This program inverts a matrix using the sub-functions above

cacheSolve <- function(x, ...) {
      ## getting the cashed matrix
      i <-  makeCacheMatrix$getinv()
	#verifying input is matrix
	if(!is.matrix) stop("Cashed variable is not a matrix"
	#verifying the cashed matrix exist
      if(!is.null(i)) {
      	message("getting cached data")
      	return(i)
      }
	##verifying input is matrix
	if(!is.matrix) stop("Cashed variable is not a matrix"
      data <- makeCacheMatrix$get()
	##inverting the matrix
      i <-solve(data)
	##cashes inverted matrix
      makeCacheMatrix$setinv(i)
      i

}