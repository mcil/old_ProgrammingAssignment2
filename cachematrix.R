# function to create a test square matrix where number is the dimension
createx <- function(number){
      set.seed(1)
      createx <- matrix(rnorm(number*number),number,number)
      createx
}
# funtion for matrix equality test
# for TRUE both arguments must be a matrix, matrix must be same size and the elements must match
ismatequal <- function(x, y){
      is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) 
}
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      m <- NULL
      setmatrix = function(y) { #set the value of the matrix
            m <<- y ## caches the inputted matrix 
            inv <<- NULL # # sets the value of the matrix inverse 
      }      
      getmatrix = function(){
             x
      }
      setinverse = function(x){
             inv <<- solve(x)
      }      
      getinverse = function(x){
            inv
      }
      list(setmatrix = setmatrix,
           getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
      }

# 
# 
# # cacheSolve: This function computes the inverse of the special "matrix" returned by 
# # makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
# # changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function (x, ...) {
      inv <- x$getinverse()    # get current value of inv
      # if an inverse has already been calculated this gets it
      if((!is.null(inv))) {
            message("getting cached data")
            return(inv) # and exit function
      }
      # otherwise calculate a new inversion
            y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
            x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
            inv <- solve(y, ...) # compute the value of the inverse of the input matrix
            x$setinverse(inv) # run the setinverse function on the inverse to cache the inverse
            inv # return the inverse
      }

test = function(mat){ # mat is the matrix dimension
      ## make mat: an invertible matrix
      mat<-createx(mat)
      
      temp = makeCacheMatrix(mat)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
      
      start.time = Sys.time()
      cacheSolve(temp)
      dur = Sys.time() - start.time
      print(dur)
}
