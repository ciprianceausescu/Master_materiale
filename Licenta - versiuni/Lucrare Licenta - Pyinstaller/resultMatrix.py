#imgType == 1 Test profesor
#imgType == 2 Test elev
# Importarea librariilor necesare
import numpy as np
import cv2
from cropTest import cropTest
from computeMatrix import computeMatrix
# Definim mai jos functia resultMatrix, care primeste ca parametru imgName - asociat
# numelui grilei pentru care dorim sa gasim matricea cu rowNum randuri si colNum coloane
# de raspunsuri, si un parametru imgType care va reprezenta: imgType == 1 Test profesor
# si imgType == 2 Test elev
def resultMatrix(imageName, rowNum, colNum, imgType):
    # Variabilele folosite in functia resultMatrix
    THRESH_VALUE = 200
    kernelSharpening = np.array([[-1, -1, -1], [-1, 9, -1], [-1, -1, -1]])
    # Imaginea salvata in variabila image, va primi valoarea rezultata de catre functia cropTest
    image = cropTest(imageName, rowNum, colNum)
    # Asupra imaginii aplicam un filtru de sharpening
    image = cv2.filter2D(image, -1, kernelSharpening)
    # Transformam imaginea din imagine RGB in imagine cu tonuri de gri, folosid functia cvtColor
    # care primeste ca parametru imaginea pe care o dorim schimbata, si un parametru care indifica
    # modul in care trebuie schimbata aceasta (BGR2GRAY - din BGR to GRAY)
    grayImage = cv2.cvtColor(image, cv2.COLOR_RGB2GRAY)
    # Se aplica un filtru Gaussian de blurare a imaginii, de dimensiune 3x3
    grayImage = cv2.GaussianBlur(grayImage, (3, 3), 0)
    # In functie de tipul imaginii, se vor realiza operatii diferite
    if imgType == 1:
        # Daca imaginea reprezinta grila profesorului, aceasta va fi tranformata in valori de 0/1
        # aplicand parametrul cv2.THRESH_BINARY
        _, threshImage = cv2.threshold(grayImage, THRESH_VALUE, 255, cv2.THRESH_BINARY)
    if imgType == 2:
        # Daca imaginea reprezinta grila elevului, ii vom aplica un edge detector
        edgedImage = cv2.Canny(grayImage, 10, 250)
        # Vom construi un nucleu 'kernel' folosind functia getStructuringElement care va returna un element de o
        # anumita dimensiune si forma pentru anumite operatii morfologice. Folosind acest kernel, se casutele in
        # care se vor afla raspunsurile elevului, vor deveni pline de culoare, exact ca cele ale profesorului
        kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (7,7))  # val 7, 7
        # In continuare vom aplica nucleul creat la pasul anterior intr-o transformare morfologica de tipul specificat
        # prin parametrul MORPH_CLOSE (operatie de inchidere), asupra imaginii produse de aplicarea edge detectorului
        # Canny. Functia folosita pentru aceasta transformare este morphologyEx
        threshImage = cv2.morphologyEx(edgedImage, cv2.MORPH_CLOSE, kernel)
    # Vom declara o matrice cu rowNum randuri, si colNum coloane, de elemente naturale. Fiecare element
    # pentru inceput va avea valoarea 0
    resultMatrix = np.zeros((rowNum, colNum),dtype=np.uint8)
    # In vectorul vectorResults vom avea un vector, in care fiecare element va avea valoarea raspunsului
    # pentru intrebarea de la pasul respectiv
    vectorResults = computeMatrix(threshImage, rowNum, colNum)
    # Elementul de pe linia i si coloana egala cu pozitia pe care se afla raspunsul bifat, va primi valoarea 1,
    # adica. Liniile matricei vor avea forma (0,0,0,1,0,0...0) in care pentru o astfel de linie, raspunsul de
    # gaseste pe pozitia 4 (3 pentru ca vectorul incepe indexarea de la val 0)
    for i in range (0,len(resultMatrix)):
        if vectorResults[i]!=-1:
            resultMatrix[i,vectorResults[i]]=1
    # Se returneaza matricea astfel obtinuta
    return resultMatrix
