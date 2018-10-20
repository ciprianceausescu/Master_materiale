# Importarea librariilor necesare
import numpy as np
from resultPosition import resultPosition
# Metoda de mai jos returneaza un vector cu pozitiile raspunsurilor bifate de catre elev
# Exemplu: [0, 1, 2, 3, 0] reprezinta un vector pentru un test cu 5 intrebari, la care raspunsurile sunt
# A, B, C, D, A
# Vectorul va fi folosit pentru construirea matricilor de raspunsuri bifate pe lucrare
def computeMatrix(image, rowNum, colNum):
    # Declarare variabile
    # Dimensiunile imaginii
    height, width = image.shape[:2]
    contorVectorPos = 0
    # rowCount = cate randuri va avea fiecare rowVector (height/nrRaspunsuri)
    rowCount = int(height / rowNum)
    # vectorPos[position] = value inseamna ca pe pozitia 'position' raspunsul corect este 'value' (0, 1, 2 sau 3)
    vectorPos = np.zeros(rowNum, dtype=np.int8)
    # Vectorul pentru randul pentru care vom cauta raspunsul bifat la pasul respectiv
    rowVector = np.zeros((rowCount, width), dtype=np.uint8)
    # Se va extrage fiecare rand cu raspunsuri din imagine, si se va gasi pozitia pe care
    # se afla raspunsul in acel rand, apeland functia resultPosition
    for i in range(0,rowNum):
        for j in range(0,rowCount):
            rowVector[j,:] = image[rowCount * i + j,:]
        vectorPos[contorVectorPos] = resultPosition(rowVector, colNum)
        contorVectorPos = contorVectorPos + 1
    return vectorPos

