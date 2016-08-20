## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.
##   - makeCacheMatrix
##   - cacheSolve
##
# #Use the below functions as follows:
# #1. Put the function in a variable
# MyMatrix <- makeCacheMatrix()
# #2. Save a matrix to the variable
# MyMatrix$set(matrix(1:4, 2, 2))
# #3. Check that the matrix was correctly saved
# CopyOfMyMatrix = MyMatrix$get()
# #4. Create the inverse of the matrix and store it in cache
# InverseMatrix = cacheSolve(MyMatrix)
# #5. Try to create it again. We will get a message that the matrix from the cache was used.
# CopyOfInverseMatrix = cacheSolve(MyMatrix)


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  MyInverseMatrix <- NULL
  
  # store the matrix in a cached variable
  set <- function(y) {
    x <<- y
    
    # There is a new matrix stored, so delete the old inverse matrix, which has to be recomputed
    MyInverseMatrix <<- NULL
  }
  
  # Get the cached matrix
  get <- function()
    x
  
  # Store the inverse matrix in a cached variable
  setmatrix <- function(solve)
    MyInverseMatrix <<- solve
  
  # Get the cached inverse matrix
  getmatrix <- function()
    MyInverseMatrix
  
  list(
    set = set,
    get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m_local <- x$getmatrix()
  
  # Is there a matrix in the cache?
  if (!is.null(m_local)) {
    # Yes, there is! Show a message.
    message("Inverse matrix taken from the cache")
    
    # Returning the cached inverse matrix breaks us out of this method.
    return(m_local)
  }
  
  # there was no matrix in the cache, so compute the inverse
  data <- x$get()
  m_local <- solve(data, ...)
  
  # store the inverse matrix in the cache
  x$setmatrix(m_local)
  
  # return the freshly computed inverse matrix
  m_local
}

