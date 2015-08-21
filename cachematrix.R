## Caching the Inverse of a Matrix - 
## The function will create an Inverse matrix by caching the input matrix.

## This Function will create a special matrix by caching the input matrix.

makeCacheMatrix <- function(x = matrix()) {
         ## creating a matrix 'x'
         m<-NULL
         set<-function(y){     
         x<<-y 
         m<<-NULL
         }
         get<-function()x
         setmatrix<-function(solve) m<<-solve
         getmatrix<-function()m
         list(set=set,get=get,
              setmatrix=setmatrix,
              getmatrix=getmatrix)
         }


## cacheSolve - This function will return the Inverse for above matrix by solving the cache.

cacheSolve<-function(x=matrix(),...){
      ## Return a matrix that is the inverse of 'x'
      m<-x$getmatrix()
      if(!is.null(m)){
          message("getting cached data")
          return(m)
      }
      matrix<-x$get
      matrix<-x$get()
           m<-solve(matrix,...)
           x$setmatrix(m)
      m
      }

