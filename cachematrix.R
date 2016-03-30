##creates a "special" vector.. a list of objects (reference Object Oriented Programming) containing function objects and variables that are saved in cache memory through <<- in order to "use" them through lexical scoping
makeCacheMatrix<-function(M=numeric())
{
    InM<-NULL                           
    set<-function(y){                   
        M<<-y                             
        InM<<-NULL                        
    }
    get<-function() M                   
    setInv<-function(solve) InM<<-solve     
    getInv<-function() InM              
    list(set=set, get=get,setInv=setInv,getInv=getInv)
}  

##function that calculates inverse matrix
cacheSolve<-function(M,..)           
{
    InM<-M$getInv()                    
    if(!is.null(InM) ){                
        message("Already calculated and in cache memory:")
        return(InM)
    }
    data<-M$get()                      
    InM<-solve(data)
    M$setInv(InM)                     
    InM
}

##example of use of functions - x inversable matrix
# x <- matrix( c(0,1,1,0), nrow=2, ncol=2)
# cache_matrix <- makeCacheMatrix(x)
# cacheSolve(cache_matrix)
##2nd call to verify that if already calculated it prints out the corresponding message and gives cached value
# cacheSolve(cache_matrix)