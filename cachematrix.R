## 逆矩阵通常是费时费力的计算，缓存逆矩阵可能会比对其进行重复计算更有利。
## 下面是可以缓存逆矩阵的函数对。

## makeCacheMatrix函数用于创建可缓存逆矩阵的特殊“矩阵”对象。

makeCacheMatrix <- function(x = matrix()) {  ## 定义x，默认值matrix()
    m <- NULL                                ## 定义m，初始值为NULL；
    set <- function(y) {                     ## 定义函数set，
        x <<- y                              ## set函数能把x的值修改为y，
        m <<- NULL                           ## 同时把m重置成NULL；
    }
    get <- function() x                      ## 定义函数get，get函数能返回x；x为自由变量，
                                             ## 因此会从get函数被定义的环境中查找x的值；
    setsolve <- function(solve) m <<- solve  ## 定义函数setsolve，setsolve函数能把m的值修改为solve；

    getsolve <- function() m                 ## 定义函数getsolve，getsolve能函数返回m；
                                             ## m为自由变量，因此会从getsolve函数被定义的环境中查找m的值
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)                ## makeCacheMatri函数返回一个list
}


## cachesolve函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
## 如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵。

cacheSolve <- function(x, ...) {             ## 读入makeCacheMatri函数生成的list
    m <- x$getsolve()                        ## 尝试读取缓存的逆矩阵
    if(!is.null(m)) {                        ## 如果缓存值不会空
        message("getting cached data")
        return(m)                            ## 直接返回缓存值
    }
                                             ## 如果代码执行到这里，说明之前的if语句被跳过，缓存值为空
    data <- x$get()                          ## 读取缓存的matrix
    m <- solve(data, ...)                    ## 求matrix的逆矩阵
    x$setsolve(m)                            ## 把平均值缓存到x的环境中
    m                                        ## 返回逆矩阵
}