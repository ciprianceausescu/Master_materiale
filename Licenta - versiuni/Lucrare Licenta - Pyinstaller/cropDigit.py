# Importarea librariilor necesare
import cv2
import numpy as np
# Metoda cropDigit decupeaza interiorul casutei in care se afla digit-ul pentru imaginea primita ca parametru si salveaza
# aceasta imagine in 'digit.jpg'
def cropDigit(imageName):
    ok = 0
    # Citirea imaginii din care vom decupa digit-ul
    image = cv2.imread(imageName)
    # Transformarea imaginii din RGB in imagine in tonuri de gri
    grayImage = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    # Blurarea imaginii folosind filtrul Gaussian de dimensiune 3x3
    grayImage = cv2.GaussianBlur(grayImage, (3, 3), 0)
    # Detectarea contururilor din imagine folosind metoda Canny
    edgedImage = cv2.Canny(grayImage, 10, 250)
    # Definirea nucleului (are forma de matrice 3x3 cu elementele egale cu 1)
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    # Se defineste imaginea pentru care se aplica operatia morfologica de inchidere a contururilor
    closedImage = cv2.morphologyEx(edgedImage, cv2.MORPH_CLOSE, kernel)
    # Detectare contururilor din imaginea asupra careia s-a aplicat operatia morfologica
    (_, contours, _) = cv2.findContours(closedImage.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    # Se va itera prin toate contururile
    for contour in contours:
        # Vom aproxima conturul
        perimeter = cv2.arcLength(contour, True)
        approximation = cv2.approxPolyDP(contour, 0.05 * perimeter, True)
        # cv2.drawContours(image, [approximation], -1, (255, 0, 0), 4)
        # cv2.imshow('I', image)
        # cv2.waitKey(0)
        # print(peri)
        # print(len(approx))
        # Vom lua cele mai mari 2 contururi care au 4 laturi, si perimetrul mai mare de 200
        if len(approximation) == 4 and perimeter > 200 and ok==0:
            x1 = approximation[0][0][0]
            y1 = approximation[0][0][1]
            x2 = approximation[1][0][0]
            y2 = approximation[1][0][1]
            x3 = approximation[2][0][0]
            y3 = approximation[2][0][1]
            x4 = approximation[3][0][0]
            y4 = approximation[3][0][1]
            # Vom face comparatii intre valorile celor 4 colturi detectate astfel incat valorile pointX1, pointY1 sa
            # contina punctul din stanga sus, iar pointX2, pointY2 punctul din dreapta jos
            if (x1 + y1 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x1
                pointY1 = y1
            if (x2 + y2 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x2
                pointY1 = y2
            if (x3 + y3 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x3
                pointY1 = y3
            if (x4 + y4 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x4
                pointY1 = y4
            if (x1 + y1 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x1
                pointY2 = y1
            if (x2 + y2 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x2
                pointY2 = y2
            if (x3 + y3 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x3
                pointY2 = y3
            if (x4 + y4 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x4
                pointY2 = y4
            digitImage1 = image[pointY1 + 11:pointY2 - 11, pointX1 + 11:pointX2 - 11]
            ok = 1
        if len(approximation) == 4 and perimeter > 200 and ok==1:
            x1 = approximation[0][0][0]
            y1 = approximation[0][0][1]
            x2 = approximation[1][0][0]
            y2 = approximation[1][0][1]
            x3 = approximation[2][0][0]
            y3 = approximation[2][0][1]
            x4 = approximation[3][0][0]
            y4 = approximation[3][0][1]
            # Vom face comparatii intre valorile celor 4 colturi detectate astfel incat valorile pointX1, pointY1 sa
            # contina punctul din stanga sus, iar pointX2, pointY2 punctul din dreapta jos
            if (x1 + y1 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x1
                pointY1 = y1
            if (x2 + y2 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x2
                pointY1 = y2
            if (x3 + y3 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x3
                pointY1 = y3
            if (x4 + y4 == min(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX1 = x4
                pointY1 = y4
            if (x1 + y1 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x1
                pointY2 = y1
            if (x2 + y2 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x2
                pointY2 = y2
            if (x3 + y3 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x3
                pointY2 = y3
            if (x4 + y4 == max(x1 + y1, x2 + y2, x3 + y3, x4 + y4)):
                pointX2 = x4
                pointY2 = y4
            digitImage2 = image[pointY1 + 11:pointY2 - 11, pointX1 + 11:pointX2 - 11]
    # Cod varianta noua pentru adaugarea unui chenar cu pixeli albi (5 pixeli pe latura)
    # imageFinal = np.zeros((38, 38, 3), np.uint8)
    # imageFinal[:,:] = (255,255,255)
    # Se tranforma imaginile celor doua casute decupate din RGB in tonuri de gri
    grayImageDigit1 = cv2.cvtColor(digitImage1, cv2.COLOR_BGR2GRAY)
    grayImageDigit2 = cv2.cvtColor(digitImage2, cv2.COLOR_BGR2GRAY)
    # Se blureaza imaginile folosind filtrul Gaussian
    grayImageDigit1 = cv2.GaussianBlur(grayImageDigit1, (3, 3), 0)
    grayImageDigit2 = cv2.GaussianBlur(grayImageDigit2, (3, 3), 0)
    # Se detecteaza muchiile in imaginile corespunzatoare casutelor in vederea aplicarii transformarii morfologice de inchidere
    # a muchiilor si de accentuare a detaliilor
    edgedImageDigit1 = cv2.Canny(grayImageDigit1, 10, 250)
    edgedImageDigit2 = cv2.Canny(grayImageDigit2, 10, 250)
    # Se defineste nucleul pentru transformarea morfologica de inchidere a muchiilor
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    # Se accentueaza detaliile din imaginile decupate pentru o detectie corecta a imaginii in care se afla cifra corespunzatoare
    # variantei
    closedImageDigit1 = cv2.morphologyEx(edgedImageDigit1, cv2.MORPH_CLOSE, kernel)
    closedImageDigit2 = cv2.morphologyEx(edgedImageDigit2, cv2.MORPH_CLOSE, kernel)
    # Se calculeaza intensitatile totale ale imaginilor
    sumPixels1 = np.sum(np.sum(closedImageDigit1[:]))
    sumPixels2 = np.sum(np.sum(closedImageDigit2[:]))
    # In imaginea digit.jpg vom salva imaginea cifrei, adica cea care are intesitatea maxima (pixeli albi) in urma transformarii
    # morfologice, digitImage1 sau digitImage2
    if sumPixels1 > sumPixels2:
        # Cod initial
        digitImage1 = cv2.resize(digitImage1, (28,28), interpolation=cv2.INTER_AREA)
        cv2.imwrite("images/digit.jpg", digitImage1)
        # Sfarsit cod initial
        # Cod varianta noua
        # digitImage1 = cv2.resize(digitImage1, (28,28), interpolation=cv2.INTER_AREA)
        # Am adaugat un contur de 5 pixeli albi peste tot
        # imageFinal[6:33, 6:33] = digitImage1[0:27, 0: 27]
        # imageFinal = cv2.resize(imageFinal, (28, 28), interpolation=cv2.INTER_AREA)
        # cv2.imwrite("images/digit.jpg", imageFinal)
        # Sfarsit cod varianta noua
    else:
        # Cod initial
        digitImage2 = cv2.resize(digitImage2, (28,28), interpolation=cv2.INTER_AREA)
        cv2.imwrite("images/digit.jpg", digitImage2)
        # Sfarsit cod initial
        # Cod varianta noua
        # digitImage2 = cv2.resize(digitImage2, (28, 28), interpolation=cv2.INTER_AREA)
        # Am adaugat un contur de 5 pixeli albi peste tot
        # imageFinal[6:33, 6:33] = digitImage2[0:27, 0: 27]
        # imageFinal = cv2.resize(imageFinal, (28, 28), interpolation=cv2.INTER_AREA)
        # cv2.imwrite("images/digit.jpg", imageFinal)
        # Sfarsit cod varianta noua