# Importarea librariilor necesare
# numpy = librarie fundamentala pentru calcule stiintifice in Python
# swapNumbers = functie care interschimba a, b astfel incat dupa apelul ei a <= b
# area = functie care calculeaza aria unui triunghi dat de cele 3 colturi ale sale
import cv2
from swapNumbers import *
from area import area
# Definim mai jos functia findTest care primeste ca parametru numele unei imagini si dupa
# apelul acesteia va salva in grila1.jpg si grila2.jpg cele doua teste grila existente in
# imaginea data ca parametru
def findTest(imageName, needBlur):
    # Variabilele folosite in functia findTest
    max_1 = 0
    max_2 = 0
    x1_max_2 = 0
    x1_max_1 = 0
    y1_max_2 = 0
    y1_max_1 = 0
    x2_max_2 = 0
    x2_max_1 = 0
    y2_max_2 = 0
    y2_max_1 = 0
    x3_max_2 = 0
    x3_max_1 = 0
    y3_max_2 = 0
    y3_max_1 = 0
    x4_max_2 = 0
    x4_max_1 = 0
    y4_max_2 = 0
    y4_max_1 = 0
    # Vom incarca imaginea, folosind functia imread
    image = cv2.imread(imageName)

    # Imaginea va fi redimensionata, cu ajutorul functiei resize
    image = cv2.resize(image, (620,870), interpolation=cv2.INTER_AREA)

    # Cream o copie a imaginii initiale, cu ajutorul functiei copy
    imageCopy = image.copy()

    # Transformam imaginea din imagine RGB in imagine cu tonuri de gri, folosid functia cvtColor
    # care primeste ca parametru imaginea pe care o dorim schimbata, si un parametru care indifica
    # modul in care trebuie schimbata aceasta (BGR2GRAY - din BGR to GRAY)
    grayImage = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Se aplica un filtru de blurare a imaginii
    if needBlur == 1:
        grayImage = cv2.GaussianBlur(grayImage, (3, 3), 0)

    # Dorim acum detectarea marginilor din imagine, folosind functia Canny, asupra imaginii in tonuri de gri
    edgedImage = cv2.Canny(grayImage, 10, 250)

    # Vom construi un nucleu 'kernel' folosind functia getStructuringElement care va returna un element de o
    # anumita dimensiune si forma pentru anumite operatii morfologice
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))  # val 7, 7
    # In continuare vom aplica nucleul creat la pasul anterior intr-o transformare morfologica de tipul specificat
    # prin parametrul MORPH_CLOSE (operatie de inchidere), asupra imaginii produse de aplicarea edge detectorului
    # Canny. Functia folosita pentru aceasta transformare este morphologyEx
    closedImage = cv2.morphologyEx(edgedImage, cv2.MORPH_CLOSE, kernel)

    # Vom detecta conturirile, adica liniile exterioare in imagine, pe care le vom pune
    # in variabila contours folosind metoda findContours
    (_, contours, _) = cv2.findContours(closedImage.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    # Parcurgem vectorul de contururi detectat
    for countour in contours:
    # Vom aproxima conturul
        perimeter = cv2.arcLength(countour, True)
        approximation = cv2.approxPolyDP(countour, 0.03 * perimeter, True) #0.02 era la inceput

    # Daca aproximarea pe care am creat-o asupra conturului are dimensiunea 4, adica 4 puncte,
    # inseamna ca acest contur este un patrulater, printre care si grilele pe care le cautam
        if len(approximation) == 4:
            # Desenam acest contur pe imaginea, pentru a verifica ca acesta s-a gasit cu succes
            cv2.drawContours(image, [approximation], -1, (255, 0, 0), 4)
            x1 = approximation[0][0][0]
            y1 = approximation[0][0][1]
            x2 = approximation[1][0][0]
            y2 = approximation[1][0][1]
            x3 = approximation[2][0][0]
            y3 = approximation[2][0][1]
            x4 = approximation[3][0][0]
            y4 = approximation[3][0][1]

            # Vom calcula cele doua arii maxime din imaginea data ca parametru si vom salva punctele (x,y) din stanga
            # sus si dreapta jos unde se gasesc aceste doua arii maxime
            aria = area(x1, y1, x2, y2, x3, y3) + area(x3, y3, x4, y4, x1, y1)

            if aria >= max_1:
                if max_1 >= max_2:
                    max_2 = max_1
                    x1_max_2 = x1_max_1
                    y1_max_2 = y1_max_1
                    x3_max_2 = x3_max_1
                    y3_max_2 = y3_max_1
                    x2_max_2 = x2_max_1
                    y2_max_2 = y2_max_1
                    x4_max_2 = x4_max_1
                    y4_max_2 = y4_max_1
                max_1 = aria
                x1_max_1 = x1
                y1_max_1 = y1
                x3_max_1 = x3
                y3_max_1 = y3
                x2_max_1 = x2
                y2_max_1 = y2
                x4_max_1 = x4
                y4_max_1 = y4
            elif aria >= max_2:
                max_2 = aria
                x1_max_2 = x1
                y1_max_2 = y1
                x3_max_2 = x3
                y3_max_2 = y3
                x2_max_2 = x2
                y2_max_2 = y2
                x4_max_2 = x4
                y4_max_2 = y4

    cv2.imwrite("images/output.jpg",image)

    # Vom face astfel incat (x1_max_1,y1_max_1) sa fie punctul din stanga sus, (x3_max_1,y3_max_1)
    # punctul din dreapta jos pentru prima grila, (x1_max_2,y1_max_2), respectiv (x3_max_2,y3_max_2)
    # pentru a doua grila

    A = x1_max_1+y1_max_1
    B = x2_max_1+y2_max_1
    C = x3_max_1+y3_max_1
    D = x4_max_1+y4_max_1

    if A == min(A,B,D,C):
        x1_max_1, x3_max_1 = swapNumbersLH(x1_max_1, x3_max_1)
        x1_max_2, x3_max_2 = swapNumbersLH(x1_max_2, x3_max_2)
        y1_max_1, y3_max_1 = swapNumbersLH(y1_max_1, y3_max_1)
        y1_max_2, y3_max_2 = swapNumbersLH(y1_max_2, y3_max_2)
        x2_max_1, x4_max_1 = swapNumbersLH(x2_max_1, x4_max_1)
        x2_max_2, x4_max_2 = swapNumbersLH(x2_max_2, x4_max_2)
        y2_max_1, y4_max_1 = swapNumbersHL(y2_max_1, y4_max_1)
        y2_max_2, y4_max_2 = swapNumbersHL(y2_max_2, y4_max_2)
    if B == min(A,B,D,C):
        auxX = x1_max_1
        auxY = y1_max_1
        x1_max_1 = x2_max_1
        y1_max_1 = y2_max_1
        x2_max_1 = x3_max_1
        y2_max_1 = y3_max_1
        x3_max_1 = x4_max_1
        y3_max_1 = y4_max_1
        x4_max_1 = auxX
        y4_max_1 = auxY
        auxX = x1_max_2
        auxY = y1_max_2
        x1_max_2 = x2_max_2
        y1_max_2 = y2_max_2
        x2_max_2 = x3_max_2
        y2_max_2 = y3_max_2
        x3_max_2 = x4_max_2
        y3_max_2 = y4_max_2
        x4_max_2 = auxX
        y4_max_2 = auxY

    # Daca x1_max_1 apartine primei grile si x1_max_2 apartine celei de-a doua grile, salvam grilele in ordinea aceasta
    # in imaginile grila1, respectiv grila2. Altfel, salvam grilele in ordine inversa in imaginile grila1, grila2.
    # Este foarte important pentru etapele urmatoare ca in imaginea grila1 sa fie grila de matematica, iar in grila2 sa
    # fie grila de informatica/fizica
    if x1_max_1 < x1_max_2:
        if y1_max_2 > y3_max_2:
            y1_max_2, y3_max_2 = swapNumbersLH(y1_max_2, y3_max_2)
        grille1 = imageCopy[y1_max_1:y3_max_1,x1_max_1:x3_max_1]
        grille2 = imageCopy[y1_max_2:y3_max_2,x1_max_2:x3_max_2]
        cv2.imwrite("images/grila1.jpg", grille1)
        cv2.imwrite("images/grila2.jpg", grille2)
        return x1_max_1, y1_max_1, x2_max_1, y2_max_1, x3_max_1, y3_max_1, x4_max_1,y4_max_1,x1_max_2, y1_max_2, x2_max_2, y2_max_2, x3_max_2, y3_max_2, x4_max_2, y4_max_2
    else:
        if y1_max_2 > y3_max_2:
            y1_max_2, y3_max_2 = swapNumbersLH(y1_max_2, y3_max_2)
        grille2 = imageCopy[y1_max_1:y3_max_1, x1_max_1:x3_max_1]
        grille1 = imageCopy[y1_max_2:y3_max_2, x1_max_2:x3_max_2]
        cv2.imwrite("images/grila1.jpg", grille1)
        cv2.imwrite("images/grila2.jpg", grille2)
        return x1_max_2, y1_max_2, x2_max_2, y2_max_2, x3_max_2, y3_max_2, x4_max_2, y4_max_2, x1_max_1, y1_max_1, x2_max_1, y2_max_1, x3_max_1, y3_max_1, x4_max_1, y4_max_1