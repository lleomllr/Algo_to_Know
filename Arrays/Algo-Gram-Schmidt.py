#%% Realisation du projet 
# SUJET : ALGORITHME DE GRAM-SCHMIDT ET DECOMPOSITION QR
import numpy as np
from scipy import linalg as LA



#Définition d'une fonction pour vérifier si une matrice passée en paramètre est inversible
def isInvertible(M):
    epsilon = 0.000000001
    if(LA.det(M)>-epsilon and LA.det(M)<epsilon):
        return False
    else:
        return True
    
    
#Définition d'une fonction qui construit une base orthonormée d'un ensemble de vecteur si ces derniers sont lineairement independant
def gram_schmidt(M):
    n=M.shape[1]
    Q = M[0] / LA.norm(M[0])

    for i in range(1, n):
        proj = np.zeros_like(M[i],dtype=np.float64)
        for j in range(i):
            proj += (np.dot(M[i], Q[j]) * Q[j]).astype(np.float64)
        v = M[i] - proj
        Q = np.vstack((Q, v / LA.norm(v)))
    return Q
        
    
#Définition d'une fonction qui calcule la decomposition QR
def decompo_QR(M):
    if(isInvertible(M) ):
        print("")
        n=M.shape[1]
        Q =gram_schmidt(M)
        Q_T = np.transpose(Q)
        R = np.dot(Q_T, M)
        print("La decomposition QR de la matrice est :\n")
        print("Q: \n",Q)
        print("R: \n",R)
        #assert(np.all(np.dot(Q.T,Q)==np.eye(n))) # orthogonalité de Q
        #assert(np.all(R[i,i:]==0) for i in range(n)) # R triangulaire inférieure
    elif not isInvertible(M):
        print("La matrice donnée n'est pas inversible")
    
    


A = np.array([[2,1],[4,3]])

B = np.array([[1,2],[2,4]])
decompo_QR(A)
#decompo_QR(B)
DQ,DR = LA.qr(A)
print(DQ)
print(DR)

#QR
def QR(M):
    if(isInvertible(M)): 
        Q = gram_schmidt(M)
        R = Q.T @ M
        print("La decomposition QR de la matrice est :\n")
        print("Q: \n",Q)
        print("R: \n",R)
    elif not isInvertible(M):
        print("La matrice donnée n'est pas inversible")
        
#QR 2
def QR2(M):
    if(isInvertible(M)):
        n = M.shape[1]
        #Q est une base orthonormée de la base M
        Q = gram_schmidt(M)
        #definition de R
        taille_mat = M.shape[0]
        R = zeros(taille_mat)
        #R = transposée de Q * M
        R = Q.T*M
        print("La decomposition QR de la matrice est :\n")
        print("Q: \n",Q)
        print("R: \n",R)
    elif not isInvertible(M):
        print("La matrice donnée n'est pas inversible")
        
#%% Partie pour tester mon programme 
A = np.array([[2,1],[4,3]])

B = np.array([[1,2],[2,4]])
QR(A)
QR(B)

