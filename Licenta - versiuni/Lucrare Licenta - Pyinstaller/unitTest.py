# Importarea librariilor necesare
from unittest import TestCase
from resultMatrix import resultMatrix
from countCorrectAnswers import countCorrectAnswers
from rotateImage import rotateImage
from findTestType import findTestType
from noise import *
from finalMark import finalMark
import cv2
import time
start_time = time.time()
#M_1 - 4, I_1 - 4, F_1 - 4 sunt grilele cu raspunsurile corecte de la admiterea din 2016
M_1 = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0],
       [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

M_2 = [[0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0],
       [0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0]]

M_3 = [[0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1],
       [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1]]

M_4 = [[0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0],
       [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0]]

I_1 = [[0, 0, 1, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0],
       [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]]

I_2 = [[1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1],
       [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1]]

I_3 = [[0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1],
       [0, 0, 1, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]

I_4 = [[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0],
       [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0]]

F_1 = [[0, 1, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0],
       [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0]]

F_2 = [[0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0],
       [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

F_3 = [[0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1],
       [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0]]

F_4 = [[1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1],
       [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 0]]
# Metoda de mai jos proceseaza imaginea primita ca parametru pentru a fi corectata, iar valorile determinate se salveaza
# in variabilele globale e1, e2 si testType
def processImage(imageName):
    # Se va reduce zgomotul din imaginea initiala si se vor salva coordonatele celor 8 colturi ale grilei
    removeNoiseMain('images/'+imageName,180)
    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, rotateNumber) = rotateImage('images/noiseRemoved.jpg')
    # Se va citi imaginea initiala
    imageInitial = cv2.imread('images/'+imageName)
    # Redimensionam imaginea initiala
    imageInitial = cv2.resize(imageInitial, (620, 870), interpolation=cv2.INTER_AREA)
    # Rotim imaginea in functie de parametrul rotNum returnat mai sus de functia rotateImage
    rowNum, colNum, _ = imageInitial.shape
    rotationMatrix = cv2.getRotationMatrix2D((colNum / 2, rowNum / 2), rotateNumber, 1)  # 0.1 / 0.2 pt testele scanate
    imageRotated = cv2.warpAffine(imageInitial, rotationMatrix, (colNum, rowNum))
    # Decupam grilele din imaginea initiala pe baza coordonatelor detectate din imaginea dim care am eliminat zgomotul
    grille1 = imageRotated[y1:y3, x1:x3]
    grille2 = imageRotated[y5:y7, x5:x7]
    # Salvam cele doua grile pe care le-am decupat
    cv2.imwrite("images/grilaDetectat1.jpg", grille1)
    cv2.imwrite("images/grilaDetectat2.jpg", grille2)
    # Determinam matricile de raspunsuri pentru cele doua grile determinate in grilaDetectat1 si grilaDetectat2
    global e1
    e1 = resultMatrix('images/grilaDetectat1.jpg', 15, 4, 2)
    global e2
    e2 = resultMatrix('images/grilaDetectat2.jpg', 15, 4, 2)
    # Determinam tipul grilei care a fost completata (informatica/fizica)
    global testType
    testType = findTestType('images/noiseRemoved.jpg')

class unitTest(TestCase):
    def tests(self):
        # Se proceseaza fiecare imagine
        processImage('img1_F3.jpg')
        # Matricile determinate in variabilele e1, e2 sunt corectate in functie de tipul testului cu matricile
        # corespunzatoare
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        # Se verifica daca tipul testului determinat este cel prezent in lucrare
        self.assertEqual(testType, 'Fizica')
        # Se verifica daca nota lucrarii determinate este egala cu nota primita la corectarea manuala a lucrarii
        self.assertEqual(value, 4.6)
        # Se afiseaza nota
        print("Nota: ", value)
        # Determinarea timpul necesar pentru rularea unui test
        # print("--- %s Timpul rularii in secunde ---" % (time.time() - start_time))
        processImage('img2_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img3_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img4_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img5_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img6_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img7_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img8_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img9_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img10_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img11_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img12_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img13_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.3)
        print("Nota: ", value)

        processImage('img14_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img15_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img16_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img17_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.5)
        print("Nota: ", value)

        processImage('img18_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img19_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7)
        print("Nota: ", value)

        processImage('img20_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img21_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img22_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img23_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img24_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img25_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img26_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img27_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img28_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img29_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img30_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img31_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.1)
        print("Nota: ", value)

        processImage('img32_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.6)
        print("Nota: ", value)

        processImage('img33_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img34_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img35_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img36_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img37_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img38_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img39_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.4)
        print("Nota: ", value)

        processImage('img40_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img41_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 9.4)
        print("Nota: ", value)

        processImage('img42_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img43_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img44_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img45_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img46_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img47_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img48_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img49_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img50_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img51_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img52_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img53_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img54_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img55_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img56_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img57_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img58_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.6)
        print("Nota: ", value)

        processImage('img59_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img60_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img61_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.5)
        print("Nota: ", value)

        processImage('img62_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img63_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img64_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img65_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img66_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img67_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img68_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img69_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img70_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img71_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img72_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img73_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img74_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.0)
        print("Nota: ", value)

        processImage('img75_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img76_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img77_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img78_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img79_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img80_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img81_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img82_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img83_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img84_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img85_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img86_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.6)
        print("Nota: ", value)

        processImage('img87_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img88_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img89_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img90_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img91_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img92_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img93_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img94_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img95_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img96_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img97_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img98_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img99_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img100_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img101_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img102_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img103_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img104_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img105_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img106_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img107_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img108_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img109_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img110_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.5)
        print("Nota: ", value)

        processImage('img111_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img112_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img113_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img114_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.0)
        print("Nota: ", value)

        processImage('img115_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img116_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img117_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img118_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.4)
        print("Nota: ", value)

        processImage('img119_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 2.2)
        print("Nota: ", value)

        processImage('img120_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img121_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 2.2)
        print("Nota: ", value)

        processImage('img122_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img123_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img124_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img125_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img126_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img127_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img128_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img129_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.7)
        print("Nota: ", value)

        processImage('img130_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 2.8)
        print("Nota: ", value)

        processImage('img131_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.3)
        print("Nota: ", value)

        processImage('img132_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img133_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img134_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img135_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img136_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img137_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img138_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img139_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img140_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.3)
        print("Nota: ", value)

        processImage('img141_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img142_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img143_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img144_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img145_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img146_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.3)
        print("Nota: ", value)

        processImage('img147_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img148_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.2)
        print("Nota: ", value)

        processImage('img149_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img150_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.5)
        print("Nota: ", value)

        processImage('img151_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 3.1)
        print("Nota: ", value)

        processImage('img152_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img153_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img154_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img155_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.8)
        print("Nota: ", value)

        processImage('img156_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img157_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img158_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.3)
        print("Nota: ", value)

        processImage('img159_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.1)
        print("Nota: ", value)

        processImage('img160_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img161_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img162_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img163_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img164_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 5.2)
        print("Nota: ", value)

        processImage('img165_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img166_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.6)
        print("Nota: ", value)

        processImage('img167_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img168_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img169_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img170_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.6)
        print("Nota: ", value)

        processImage('img171_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img172_F3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, F_3)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img173_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.7)
        print("Nota: ", value)

        processImage('img174_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img175_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img176_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img177_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img178_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img179_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img180_I4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, I_4)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 2.8)
        print("Nota: ", value)

        processImage('img181_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 6.4)
        print("Nota: ", value)

        processImage('img182_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 2.8)
        print("Nota: ", value)

        processImage('img183_I3.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_3) + countCorrectAnswers(e2, I_3)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 4.9)
        print("Nota: ", value)

        processImage('img184_I1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, I_1)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 3.1)
        print("Nota: ", value)

        processImage('img185_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 5.5)
        print("Nota: ", value)

        processImage('img186_F4.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_4) + countCorrectAnswers(e2, F_4)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img187_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 8.8)
        print("Nota: ", value)

        processImage('img188_F2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, F_2)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.9)
        print("Nota: ", value)

        processImage('img189_I2.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_2) + countCorrectAnswers(e2, I_2)))
        self.assertEqual(testType, 'Informatica')
        self.assertEqual(value, 7.0)
        print("Nota: ", value)

        processImage('img190_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)

        processImage('img191_F1.jpg')
        value = float(finalMark(countCorrectAnswers(e1, M_1) + countCorrectAnswers(e2, F_1)))
        self.assertEqual(testType, 'Fizica')
        self.assertEqual(value, 7.6)
        print("Nota: ", value)