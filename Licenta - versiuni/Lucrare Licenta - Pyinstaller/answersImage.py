# Importarea librariilor necesare
from cropTest import cropTest
from resultMatrix import resultMatrix
from noise import removeNoiseMain
from findTestType import *
import cropDigit as cD
from performRecognition import predictNumber
import time
from finalMark import finalMark
from countCorrectAnswers import countCorrectAnswers
# Functia answersImage corecteaza lucrarea trimisa ca parametru si afiseaza imaginea cu rezultatul astfel: daca
# s-a raspuns corect la o intrebare, randul aferent acesteia se va colora cu verde, iar daca nu, randul se va colora
# cu rosu
def answersImage(imageName, passFactor, M_1, M_2, M_3, M_4, I_1, I_2, I_3, I_4, F_1, F_2, F_3, F_4):
    # Timpul la care a inceput sa ruleze metoda answersImage
    startTime = time.time()
    # Se va reduce zgomotul din imaginea initiala
    removeNoiseMain(imageName, passFactor)
    # Se vor extrage cele 8 colturi care apartin grilelor din lucrarea care se corecteaza, dar si parametrul de rotire
    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, rotNum) = rotateImage('images/noiseRemoved.jpg')
    # Se va citi imaginea initiala
    imageInitial = cv2.imread(imageName)
    # Determinam dimensiunile imaginii initiale
    imageInitialWidth = imageInitial.shape[1]
    imageInitialHeight = imageInitial.shape[0]
    # Determinam regiunea unde se gasesc cele doua casute pentru Informatica / Fizica
    findTestType2('images/noiseRemoved.jpg', imageName, imageInitialWidth, imageInitialHeight)
    # Se determina tipul grilei Informatica sau Fizica
    testType = findTestType('images/noiseRemoved.jpg')
    # Decupam digit-ul din regiunea de sus sau de jos, in functie de grila la care raspunde elevul Informatica / Fizica,
    # pe care il vom salva ulterior in digit.jpg
    cD.cropDigit("images/imgType.jpg")
    # Vom determina valoarea digit-ului, apeland functia predictNumber din performRecognition asupra imaginii digit.jpg
    predictedDigit = predictNumber("images/digit.jpg")
    # Se afiseaza valoarea digit-ului detectat
    print ("Cifra detectata:", predictedDigit)
    # In functie de tipul grilei si de digit-ul detectat se vor selecta matricile de raspunsuri corespunzatoare,
    # necesare pentru verificarea raspunsurilor elevului
    if (testType == 'Informatica' and predictedDigit == 1):
        matrix1 = M_1
        matrix2 = I_1
    if (testType == 'Informatica' and predictedDigit == 2):
        matrix1 = M_2
        matrix2 = I_2
    if (testType == 'Informatica' and predictedDigit == 3):
        matrix1 = M_3
        matrix2 = I_3
    if (testType == 'Informatica' and predictedDigit == 4):
        matrix1 = M_4
        matrix2 = I_4
    if (testType == 'Fizica' and predictedDigit == 1):
        matrix1 = M_1
        matrix2 = F_1
    if (testType == 'Fizica' and predictedDigit == 2):
        matrix1 = M_2
        matrix2 = F_2
    if (testType == 'Fizica' and predictedDigit == 3):
        matrix1 = M_3
        matrix2 = F_3
    if (testType == 'Fizica' and predictedDigit == 4):
        matrix1 = M_4
        matrix2 = F_4
    # Redimensionam imaginea initiala
    imageInitial = cv2.resize(imageInitial, (620, 870), interpolation=cv2.INTER_AREA)
    # Rotim imaginea in functie de parametrul rotNum returnat mai sus de functia rotateImage
    rowNum, colNum, _ = imageInitial.shape
    # Se creaza matricea de rotatie centrata in mijlocul imaginii, cu unghiul dat de parametrul rotNum
    rotationMatrix = cv2.getRotationMatrix2D((colNum/ 2, rowNum / 2), rotNum, 1)  # 0.1 / 0.2 pt testele scanate
    rotatedImage = cv2.warpAffine(imageInitial, rotationMatrix, (colNum, rowNum))
    # Decupam grilele din imaginea initiala care a fost rotita, pe baza coordonatelor detectate din imaginea din care
    # am eliminat zgomotul
    grille1 = rotatedImage[y1:y3, x1:x3]
    grille2 = rotatedImage[y5:y7, x5:x7]
    # Salvam cele doua grile pe care le-am decupat
    cv2.imwrite("images/grilaDetectat1.jpg", grille1)
    cv2.imwrite("images/grilaDetectat2.jpg", grille2)
    # Determinam matricile de raspunsuri pentru cele doua grile salvate in grilaDetectat1 si grilaDetectat2
    e1 = resultMatrix('images/grilaDetectat1.jpg', 15, 4, 2)
    e2 = resultMatrix('images/grilaDetectat2.jpg', 15, 4, 2)
    # Calcularea notei finale
    testMark = finalMark((countCorrectAnswers(e1, matrix1) + countCorrectAnswers(e2, matrix2)))
    # Se declara un vector de rezultate in care valoarea 1 inseamna raspuns corect / 0 raspuns gresit
    resultVector = np.zeros(15,dtype=np.uint8)
    # Se vor parcurge matricile matrix1 si e1 in paralel pentru a vedea care raspunsuri au fost bifate corect
    # Pentru fiecare rand din matricile matrix1 si e1
    for i in range (0,len(matrix1)):
        # Daca randurile sunt identice, i.e s-a bifat raspunsul corect, pozitia i din vectorul de raspunsuri va primi
        # valorea 1, altfel va ramane 0
        if np.array_equal(matrix1[i],e1[i]):
            resultVector[i] = 1
    # Se decupeaza regiunea in care se afla doar grila
    img = cropTest('images/grilaDetectat1.jpg',15,4)
    # Se iau dimensiunile imaginii care a fost decupata
    height, width = img.shape[:2]
    # Se parcuge vectorul de raspunsuri care a fost creat anterior
    for i in range(0,15):
        # Daca pe pozitia i valoarea este 1, inseamna ca s-a bifat raspunsul corect, si randul se va colora cu verde
        # (0, 255, 0) - BGR
        if resultVector[i] == 1:
            img[i * int(height / 15):i * int(height / 15) + 3, :] = (0, 255, 0)
            img[(i+1) * int(height / 15) - 3 : (i+1) * int(height / 15) , :] = (0, 255, 0)
            img[i*int(height / 15):(i+1)*int(height / 15),0:3] = (0, 255, 0)
            img[i * int(height / 15):(i + 1) * int(height / 15), width - 3 : width] = (0, 255, 0)
        # Daca pe pozitia i valoarea este diferita de 1, inseamna ca nu s-a bifat raspunsul corect, si randul se va
        # colora cu rosu (0, 0, 255) - BGR
        else:
            img[i * int(height / 15):i * int(height / 15) + 3 , :] = (0, 0, 255)
            img[(i + 1) * int(height / 15) - 3: (i + 1) * int(height / 15), :] = (0, 0, 255)
            img[i * int(height / 15):(i + 1) * int(height / 15), 0:3] = (0, 0, 255)
            img[i * int(height / 15):(i + 1) * int(height / 15), width - 3 : width] = (0, 0, 255)
    # Se detecteaza pozitiile la care se afla zona grilei care a fost decupata si in imaginea initiala se plaseaza
    # imaginea img, a carei randuri sunt inconjurate de verde sau rosu
    img = cv2.resize(img, (x3-x1-71,y3-y1-34), interpolation=cv2.INTER_AREA)
    rotatedImage[y1 + 34:y3, x1 + 71:x3] = img
    # Se declara un vector de rezultate in care valoarea 1 inseamna raspuns corect / 0 raspuns gresit
    resultVector = np.zeros(15, dtype=np.uint8)
    # Se vor parcurge matricile matrix2 si e2 in paralel pentru a vedea care raspunsuri au fost bifate corect
    # Pentru fiecare rand din matricile matrix2 si e2
    for i in range(0, len(matrix2)):
        # Daca randurile sunt identice, i.e s-a bifat raspunsul corect, pozitia i din vectorul de raspunsuri va primi
        # valorea 1, altfel va ramane 0
        if np.array_equal(matrix2[i], e2[i]):
            resultVector[i] = 1
    # Se decupeaza regiunea in care se afla doar grila
    img = cropTest('images/grilaDetectat2.jpg', 15, 4)
    # Se iau dimensiunile imaginii care a fost decupata
    height, width = img.shape[:2]
    # Se parcuge vectorul de raspunsuri care a fost creat anterior
    for i in range(0, 15):
        # Daca pe pozitia i valoarea este 1, inseamna ca s-a bifat raspunsul corect, si randul se va colora cu verde
        # (0, 255, 0) - BGR
        if resultVector[i] == 1:
            img[i * int(height / 15):i * int(height / 15) + 3, :] = (0, 255, 0)
            img[(i + 1) * int(height / 15) - 3: (i + 1) * int(height / 15), :] = (0, 255, 0)
            img[i * int(height / 15):(i + 1) * int(height / 15), 0:3] = (0, 255, 0)
            img[i * int(height / 15):(i + 1) * int(height / 15), width - 3: width] = (0, 255, 0)
        # Daca pe pozitia i valoarea este diferita de 1, inseamna ca nu s-a bifat raspunsul corect, si randul se va
        # colora cu rosu (0, 0, 255) - BGR
        else:
            img[i * int(height / 15):i * int(height / 15) + 3, :] = (0, 0, 255)
            img[(i + 1) * int(height / 15) - 3: (i + 1) * int(height / 15), :] = (0, 0, 255)
            img[i * int(height / 15):(i + 1) * int(height / 15), 0:3] = (0, 0, 255)
            img[i * int(height / 15):(i + 1) * int(height / 15), width - 3: width] = (0, 0, 255)
    # Se detecteaza pozitiile la care se afla zona grilei care a fost decupata si in imaginea initiala se plaseaza
    # imaginea img, a carei randuri sunt inconjurate de verde sau rosu
    img = cv2.resize(img, (x7 - x5 - 71, y7 - y5 - 34), interpolation=cv2.INTER_AREA)
    rotatedImage[y5 + 34:y7, x5 + 71:x7] = img
    # Imaginea rotatedImage se redimensioneaza la 650x470 si se afiseaza catre utilizator
    rotatedImage = cv2.resize(rotatedImage, (470, 650), interpolation=cv2.INTER_AREA)
    # Se afiseaza pe lucrarea care se corecteaza tipul grilei rezolvat, numarul variantei care a fost detectat si nota
    # finala conform variantei
    cv2.putText(rotatedImage, testType + " Var:" + str(predictedDigit) + " Nota:" + str(testMark), (10, rotatedImage.shape[0]-15),
                        cv2.FONT_HERSHEY_DUPLEX, 1, (255, 0, 0), 2)
    # Se afiseaza timpul in care a rulat aceasta functie
    print("---Timpul rularii in secunde: %s ---" % (time.time() - startTime))
    # Se va afisa utilizatorului imaginea care a fost corectata
    cv2.imshow("CTI", rotatedImage)
    cv2.waitKey(0)