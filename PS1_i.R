#HW1 Haixiang Zeng 12032760

#1.flowchart

Print_values <- function(a, b, c){
  a <- runif(1,1,100)
  b <- runif(1,1,100)
  c1 <- runif(1,1,100)
  
  if(a>b && b>c){
    cat(a, b, c)
  }
  if(a<b && b<c){
    cat(c, b, a)
  }
  if(a>b && b<c){
    if(a>c){
      cat(a, c, b)
    }else{
      cat(c, a, b)
    }
  }
  if(a<b && b>c){
    if(a>c){
      cat(b, a, c)
    }else{
      cat(b, c, a)
      }
  }
}
Print_values(a, b, c)

#can`t print different variables in one row by using "print"
#After reading https://cloud.tencent.com/developer/ask/45564, use "cat" instead. 

#2.matrix multiplication
#2.1
vector1 <- c(runif(50,0,50))
vector2 <- as.integer(vector1)
M1 <- matrix(vector2, nrow = 5, ncol = 10)
vector3 <- c(runif(50,0,50))
vector4 <- as.integer(vector3)
M2 <- matrix(vector4, nrow = 10, ncol = 5)
M1
M2
#2.2
M3 <- array(0, dim = c(5,5))
Matrix_multip <- function(M1, M2){
  for (i in 1:5) {
    for (j in 1:5) {
   M3[i,j] <- sum(M1[i,]*M2[,j])
   
    }
  }
  return(M3)
}
Matrix_multip(M1,M2)
print(M3)

#the way to verify.
Matrix_verify <- function(M1, M2){
  M3 <- M1%*%M2 
  print(M3)
}
Matrix_verify(M1,M2)

#3
