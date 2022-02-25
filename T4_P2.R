#Descomposición LU

LU= function(A){
  
  renglones= NROW(A)
  L= diag(1,renglones)
  print("VAMOS A COMENZAR LA DESCOMPOSICION LU")
  for(i in 1:(renglones-1)){
    
    for (j in (i+1):renglones){
      
      factor1= -A[j,i]/A[i,i]
      factor2= A[j,i]/A[i,i]
      
      
      A[j,]= (A[j,]+((factor1)*A[i,]))
      
      L[,i]=(L[,i]+((factor2)*L[,j]))
      print(paste0("Renglon ",j," +",factor1," Renglon ",i))
      print(paste0("Columna ",i," +",factor2," Columna ",j))
      
      print("Matriz L")
      print(L)
      print("Matriz u")
      print(A)
      
    }
  }
  
  U=A
  
  #print(L)
  #print(U)
  return(list(L,U))
}

#Descomposición PA=LU

PALU= function(A){
#Se guarda valor de A2 porque se cambiarán los renglones de la matriz A.  
A2= A

renglones= NROW(A)
  
P= diag(1,renglones)
  
# Escalonar por pivoteo parcial
  
  for (i in 1:(renglones-1)) {
    vect= A[i:renglones,i]
 indice= which.max(abs(vect))+(i-1)
#Identidad 
 p_i=diag(1,renglones)
 
 #Permutacion
 
if(i!=indice){
  
  #Cambio para pivoteo parcial
   auxiliar = A[i,]
   A[i,]= A[indice,]
  A[indice,]= auxiliar
  
  #Cambio para matriz de permutación
  auxiliar2= p_i[i,]
  p_i[i,]= p_i[indice,]
  p_i[indice,]= auxiliar2
  
#Matriz P 
P= p_i %*% P

}
    #Volver a diagonal con valor 1
#Se guardo valor para hacer 0 más adelante    
aux3= A[i,]/ A[i,i]
    
    
    if(i!=renglones){
      for (j in (i+1):renglones) {
        if (A[j,i]!=0){
          A[j,]= (A[j,]/A[j,i])- (aux3)
        }
        
       
      } 
    }
    
  }
  
print(P)
print(A2)

LU(P%*%A2)

}

#Prueba
pm= matrix(c(0,-1,3,0,5,6,2,-2,7),nrow=3)
PALU(pm)
