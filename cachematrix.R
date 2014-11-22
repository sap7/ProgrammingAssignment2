## 
#makeCacheMatrix() stores the cache of a matrix and its inverse 
#cacheSolve() retrieves the inverse from cache if already computed else computes the inverse for the 1st time

## Cache the matrix and its inverse and return a list with 3 elements that can be used to access the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  mCache <<- x;
  mInv<<-NULL;
  
  get <- function() {mCache}
  getInv<-function(){mInv}
  solveInv<-function(y) {
    mInv<<-solve(y);
    mInv
  }
  list( get = get,getInv = getInv, solve = solveInv);
  
}


## Return the inverse of the matrix from cache if already computed else compute and store in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInvCache<-x$getInv();
  if(!is.null(xInvCache)){
    print("Returning inverse from cache ...");
    return (xInvCache);
  }else{
    
    data<-x$get();
    print("Solving inverse 1st time ...")
    
    xInv<-x$solve(data) 
    return (xInv)
    
  }
  
}
