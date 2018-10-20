# Importarea librariilor necesare
import numpy as np
# Metoda de mai jos determina pozitia pe care se afla bifa in vectorul de pixeli primit ca parametru
# Pozitia bifei este aceea in care suma pixelilor este maxima
def resultPosition(vector, colNum):
    # Se va extrage numarul de pixeli din fiecare celula de raspuns
    colCount = int(len(vector[0]) / colNum)
    maxSum = 0
    minSum = 1000000
    answerPosition = 0
    # Pentru fiecare celula de raspuns, se va calcula suma tuturor pixelilor din aceasta
    # se va salva suma maxima in val maxSum, si pozitia pe care se va afla aceasta suma maxima
    # se va returna prin variabila position
    for i in range (0,colNum):
        sumPixels = np.sum(np.sum(vector[:, i * colCount: i * colCount + colCount - 1]))
        if minSum > sumPixels:
            minSum = sumPixels
        if maxSum < sumPixels:
            maxSum = sumPixels
            answerPosition = i
    # Maximul trebuie sa aiba o valoarea considerabil mai mare in comparatie cu minimul, astfel incat
    # daca pentru o intrebare nu se bifeaza niciun raspuns, pozitia returnata sa fie -1
    if maxSum > minSum * 1.5:
        return answerPosition
    else:
        return -1