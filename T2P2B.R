Rango= function(A){
  
  renglones= NROW(A)
  columnas= NCOL(A)
  
# Escalonar 
#A[1,]= A[1,]/ A[1,1]
  
for (i in 1:renglones) {
  
#Volver a diagonal con valor 1
   
  A[i,] = A[i,]/ A[i,i]
  print(A)  
  
  if(i!=renglones){
    for (j in (i+1):renglones) {
      if (A[j,i]!=0){
      A[j,]= (A[j,]/A[j,i])-A[i,] 
    }


      
    print(A)
    } 
  }
  
}


#Rango y pivotes
  
rango= sum(diag(A))
pivotes= diag(A)
  
  print(paste("El rango es",rango))
  print(paste("Los pivotes son",pivotes))
  
  }

#PRUEBAS

B= matrix(data= c(5,3,5,7,2,7,8,10,2,10,4,2), nrow = 3,ncol = 4)
Rango(B)


C= matrix(data=sample(1:100,55),nrow = 5,ncol = 11)
Rango(C)



