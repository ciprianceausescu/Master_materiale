# Importarea librariilor necesare
import cv2
from findTest import findTest
# Functia rotateImage are rolul de a roti imaginea pana in momentul in care dreapta determinata de
# punctele (x1,y1), (x4,y4) se afla aproximativ paralela cu Ox. Valorile returnate de catre aceasta functie reprezinta
# perechile de valori (x,y) corespunzatoare celor 8 colturi ale grilelor prezente in imaginea selectata, dar si valoarea
# parametrului de rotatie al imaginii, parametru pe care il vom folosi intr-o procesare viitoare
def rotateImage(imageName):
    # Se citeste imaginea
    image = cv2.imread(imageName,1)
    rotateNumber = 0.0
    rowCount, colCount, _ = image.shape
    # Se determina coordonatele celor 8 colturi corespunzatoare grilelor existente in lucrarea care se corecteaza
    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8) = findTest(imageName, 1)
    # Daca y1 < y4 se va roti imaginea in sensul acelor de ceasornic
    if y1 < y4:
        while y1 not in range(y4-2,y4+2):
            image = cv2.imread(imageName, 1)
            rowCount, colCount, _ = image.shape
            M = cv2.getRotationMatrix2D((colCount/2,rowCount/2),0.05,1) #0.1 / 0.2 pt testele scanate
            dst = cv2.warpAffine(image,M,(colCount,rowCount))
            cv2.imwrite(imageName,dst)
            rotateNumber += 0.05
            (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8) = findTest(imageName, 0)
    # Daca y1 > y4 se va roti imaginea in sensul invers acelor de ceasornic
    if y1 > y4:
        while y1 not in range(y4-2,y4+2):
            image = cv2.imread(imageName, 1)
            rowCount, colCount, _ = image.shape
            M = cv2.getRotationMatrix2D((colCount/2,rowCount/2),-0.05,1) #-0.1 / -0.2 pt testele scanate
            dst = cv2.warpAffine(image,M,(colCount,rowCount))
            cv2.imwrite(imageName,dst)
            rotateNumber -= 0.05
            (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8) = findTest(imageName, 0)
    # Se vor returna noile coordonate ale celor 8 puncte, si valoarea cu care s-a efectuat rotirea
    return (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, round(rotateNumber, 2))




