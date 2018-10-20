# Importarea librariilor necesare
from rotateImage import rotateImage
import numpy as np
import cv2
# Functia findTestType se foloseste pentru a determina tipul de test completat de catre elev, Informatica sau Fizica
# Aceasta functie se foloseste in testele unitare

def findTestType(imgName):
    # Se defineste un filtu de sharpening al imaginii
    kernelSharpening = np.array([[-1, -1, -1], [-1, 9, -1], [-1, -1, -1]])
    # Se citeste imaginea
    image = cv2.imread(imgName)
    # Se redimensioneaza imaginea
    image = cv2.resize(image, (620, 870), interpolation=cv2.INTER_AREA)
    # Se determina cooronatele celor 8 colturi ale grilelor existente in imagine
    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, rotateNumber) = rotateImage(imgName)
    # Se decupeaza locul in care se gasesc cele doua casute pentru bifa de Fizica / Informatica
    imageTestType = image[y5 - 65:y5 - 3, x7 - 35:x7 + 10]
    # Se aplica imaginii filtrul de accentuare a detaliilor, kernelSharpening, definit anterior
    imageTestType = cv2.filter2D(imageTestType, -1, kernelSharpening)
    # Se prelucreaza acesta imagine decupata pentru calcularea maximului intensitatilor pixelilor
    # Se transforma imaginea din RGB in imagine in tonuri de gri
    grayImageTestType = cv2.cvtColor(imageTestType, cv2.COLOR_BGR2GRAY)
    # Se aplica un filtru de blurare a imaginii
    grayImageTestType = cv2.GaussianBlur(grayImageTestType, (3, 3), 0)
    # Dorim acum detectarea marginilor din imagine, folosind functia Canny, asupra imaginii in tonuri de gri
    edgedImageTestType = cv2.Canny(grayImageTestType, 10, 250)
    # Vom construi un nucleu 'kernel' folosind functia getStructuringElement care va returna un element de o
    # anumita dimensiune si forma pentru anumite operatii morfologice
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    # In continuare vom aplica nucleul creat la pasul anterior intr-o transformare morfologica de tipul specificat
    # prin parametrul MORPH_CLOSE (operatie de inchidere), asupra imaginii produse de aplicarea edge detectorului
    # Canny. Functia folosita pentru aceasta transformare este morphologyEx
    closedImageTestType = cv2.morphologyEx(edgedImageTestType, cv2.MORPH_CLOSE, kernel)

    lenY = closedImageTestType.shape[0]

    # Se calculeaza suma pixelilor din jumatatea de sus, respectiv jumatatea de jos in variabilele sumPixels1,
    # sumPixels2
    sumPixels1 = 0
    for i in range (0,int(lenY/2)):
        sumPixels1 = sumPixels1 + np.sum(np.sum(closedImageTestType[i]))
    sumPixels2 = 0
    for i in range(int(lenY / 2), lenY):
        sumPixels2 = sumPixels2 + np.sum(np.sum(closedImageTestType[i]))

    # Daca maximul este sumPixels1, adica in jumtatatea de sus => raspunsul este Informatica;
    # altfel => raspunsul este Fizica
    if sumPixels1 > sumPixels2:
        return "Informatica"
    else:
        return "Fizica"

# Functia findTestType2 primeste ca parametru numele imaginii din care s-a eliminat zgomotul, numele imaginii
# initiale, si dimensiunile imaginii initiale
def findTestType2(imageName, imageInitialName, dimX, dimY):
    # Se citeste imaginea
    image = cv2.imread(imageInitialName)

    # Se determina cooronatele celor 8 colturi ale grilelor existente in imagine
    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, _) = rotateImage(imageName)

    valueX = int((x8 * dimX) / 620)
    valueY = int((y8 * dimY) / 870)

    imageTestType = image[valueY-280:valueY-3,valueX-120:valueX+10]
    # Se decupeaza locul in care se gasesc cele doua casute pentru bifa de Fizica / Informatica, din imaginea initiala
    # care a fost citita in variabila image
    cv2.imwrite('images/imgType.jpg', imageTestType)