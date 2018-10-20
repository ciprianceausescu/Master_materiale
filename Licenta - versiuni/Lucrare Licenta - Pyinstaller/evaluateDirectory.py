# Importarea librariilor necesare
import os
from natsort import natsorted
from noise import removeNoiseMain
from resultMatrix import resultMatrix
from findTestType import *
from countCorrectAnswers import countCorrectAnswers
from finalMark import finalMark
# Metoda de mai jos primeste ca parametru numele folderului selectat de catre utilizator, parametrul passFactor, valorile
# matricilor cu care se vor compara raspunsurile si tipul grilei. Aceasta metoda va itera prin toate imaginile din folderul
# selectat si se vor corecta grilele. Aceasta vor fi corectate folosind matricile corespunzatoare parametrului grilleType
# care poate fi F_1 - F_4 sau I_1 - I_4
def evaluateDirectory(directoryPath, passFactor, M_1, M_2, M_3, M_4, I_1, I_2, I_3, I_4, F_1, F_2, F_3, F_4, grilleType):
    # Se vor extrage numele tuturor imaginilor existente in folder
    imageNameList = os.listdir(directoryPath)
    # Se vor sorta astfel incat ele sa fie in ordinea in care apar in folderul selectat
    imageNameList = natsorted(imageNameList, reverse=False)
    # Se va extrage numele folderului selectat
    dirName = directoryPath.split('/')[len(directoryPath.split('/'))-1]
    # Fisierul text in care se vor scrie rezultatele va avea denumirea folderului selectat si se va salva in folderul Rezultate
    # existent in folderul curent al aplicatiei. Daca acesta nu exista, se va crea automat
    file = open("Rezultate/"+dirName+".txt", 'w')
    # Se vor sterge datele existente in acest fisier text, in cazul in care acesta este deja creat
    file.truncate()
    # Se va itera prin fiecare lista care contine numele imaginilor
    for i in range(0, len(imageNameList)):
        # Se va reduce zgomotul din imaginea de la pasul curent, folosind parametrul passFactor
        removeNoiseMain(directoryPath + '/' + imageNameList[i], passFactor)
        # Se vor extrage colturile grilei din imaginea din care s-a redus zgomotul
        (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, rotateNumber) = rotateImage(
            'images/noiseRemoved.jpg')
        # Citim imaginea initiala
        imageInitial = cv2.imread(directoryPath + '/' + imageNameList[i])
        # Redimensionam imaginea initiala
        imageInitial = cv2.resize(imageInitial, (620, 870), interpolation=cv2.INTER_AREA)
        # Rotim imaginea in functie de parametrul rotateNumber returnat mai sus de functia rotateImage
        rowNum, colNum, _ = imageInitial.shape
        # Matricea de rotatie este creata in functie de parametrul rotateNumber
        rotateMatrix = cv2.getRotationMatrix2D((colNum / 2, rowNum / 2), rotateNumber, 1)
        # Imaginea este rotita folosind matricea de rotatie creata anterior
        imgInitialRot = cv2.warpAffine(imageInitial, rotateMatrix, (colNum, rowNum))
        # Se extrag grilele din imagine
        grille1 = imgInitialRot[y1:y3, x1:x3]
        grille2 = imgInitialRot[y5:y7, x5:x7]
        # Salvam cele doua grile pe care le-am decupat
        cv2.imwrite("images/grilaDetectat1.jpg", grille1)
        cv2.imwrite("images/grilaDetectat2.jpg", grille2)
        # Se determina matricile de raspunsuri pentru cele doua grile din imagine
        e1 = resultMatrix('images/grilaDetectat1.jpg', 15, 4, 2)
        e2 = resultMatrix('images/grilaDetectat2.jpg', 15, 4, 2)
        # In functie de parametrul grilleType matricile se vor compara corespunzator
        # In cazul in care grilleType este I_1, matricile e1, e2 se vor compara cu M_1, I_1
        if (grilleType == 'I_1'):
            N1 = countCorrectAnswers(M_1, e1)
            N2 = countCorrectAnswers(I_1, e2)
            grilleNumber = 1
            grilleTypeShow = "Informatica"
        if (grilleType == 'I_2'):
            N1 = countCorrectAnswers(M_2, e1)
            N2 = countCorrectAnswers(I_2, e2)
            grilleNumber = 2
            grilleTypeShow = "Informatica"
        if (grilleType == 'I_3'):
            N1 = countCorrectAnswers(M_3, e1)
            N2 = countCorrectAnswers(I_3, e2)
            grilleNumber = 3
            grilleTypeShow = "Informatica"
        if (grilleType == 'I_4'):
            N1 = countCorrectAnswers(M_4, e1)
            N2 = countCorrectAnswers(I_4, e2)
            grilleNumber = 4
            grilleTypeShow = "Informatica"
        if (grilleType == 'F_1'):
            N1 = countCorrectAnswers(M_1, e1)
            N2 = countCorrectAnswers(F_1, e2)
            grilleNumber = 1
            grilleTypeShow = "Fizica"
        if (grilleType == 'F_2'):
            N1 = countCorrectAnswers(M_2, e1)
            N2 = countCorrectAnswers(F_2, e2)
            grilleNumber = 2
            grilleTypeShow = "Fizica"
        if (grilleType == 'F_3'):
            N1 = countCorrectAnswers(M_3, e1)
            N2 = countCorrectAnswers(F_3, e2)
            grilleNumber = 3
            grilleTypeShow = "Fizica"
        if (grilleType == 'F_4'):
            N1 = countCorrectAnswers(M_4, e1)
            N2 = countCorrectAnswers(F_4, e2)
            grilleNumber = 4
            grilleTypeShow = "Fizica"
        # Rezultatele determinate se vor scrie in fisierul file
        resultTextFile = imageNameList[i] + " Grila: " + grilleTypeShow + " " + str(grilleNumber) + \
                         ", [" + str(N1) + " Matematica], [" + str(N2) + " " + grilleTypeShow + "]. Nota finala: " \
                         + finalMark(N1 + N2) + "\n"
        file.write(resultTextFile)
    # Metoda returneaza numele folderului selectat pe care il va afisa intr-o fereastra de informare dupa ce toate grilele
    # existente in folder s-au corectat
    return dirName
