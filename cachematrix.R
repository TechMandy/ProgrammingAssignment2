## Caching the Inverse of a Matrix - 
## The function will create an Inverse matrix by caching the input matrix.

## This Function will create a special matrix by caching the input matrix.
##Caching the Inverse of a Matrix
   ##Make Cache Matrix
## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
         ## creating a matrix 'x'
         # stores the cached value
         # initialize to NULL
         m<-NULL
         # create the matrix in the working environment
         set<-function(y){     
         x<<-y 
         m<<-NULL
         }
         # get the value of the matrix
         get<-function()x
         # invert the matrix and store in cache
         setmatrix<-function(solve) m<<-solve
         # get the inverted matrix from cache
         getmatrix<-function()m
         # return the created functions to the working environment
         list(set=set,get=get,
              setmatrix=setmatrix,
              getmatrix=getmatrix)
         }



## cacheSolve - This function will return the Inverse for above matrix by solving the cache.
 ##Caching inverse of Matrix
   ## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
   ## If the inverted matrix does not exist in cache,
   ## it it created in the working environment and it's inverted value
   ## is stored in cache
cacheSolve<-function(x=matrix(),...){
      ## Return a matrix that is the inverse of 'x'
      m<-x$getmatrix()
      # return inverted matrix from cache if it exists
            # else create the matrix in working environment
      if(!is.null(m)){
          message("getting cached data")
         # display matrix in console
          return(m)
      }
      # create matrix since it does not exist
      matrix<-x$get
      matrix<-x$get()
           m<-solve(matrix,...)
           x$setmatrix(m)
      m
      }
      
# > m<-matrix(c(2,4,9,3,5,8,1,6,7),nrow=3,ncol=3)
# > m
#       [,1] [,2] [,3]
# [1,]    2    3    1
# [2,]    4    5    6
# [3,]    9    8    7
# > solve(m)
#           [,1]       [,2]        [,3]
# [1,] -0.3333333 -0.3333333  0.33333333
# [2,]  0.6666667  0.1282051 -0.20512821
# [3,] -0.3333333  0.2820513 -0.05128205
#

# > mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
# > mat2 <- makeCacheMatrix(mat)
# > cacheSolve(mat2)
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

