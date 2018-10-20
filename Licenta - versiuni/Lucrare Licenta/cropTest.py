# Importarea librariilor necesare
import cv2

# Definim mai jos functia care primeste ca parametru numele unei imagini si returneaza zona din grila detectata in care se
# afla efectiv raspunsurile
def cropTest(imageName, rowNum, colNum):
    # In variabila image se va citi imaginea cu numele imageName
    image = cv2.imread(imageName)
    # Imaginea se va redimensiona la niste valori dorite
    image = cv2.resize(image, (125, 233), interpolation=cv2.INTER_AREA)
    height, width = image.shape[:2]
    # Din imaginea citita, vom extrage doar regiunea unde se afla raspunsurile
    image = image[28:height, 56:width]
    # Vom redimensiona imaginea astfel in fiecare casuta de raspuns sa avem 20 * 15 pixeli
    image = cv2.resize(image, (20 * colNum, 15 * rowNum), interpolation=cv2.INTER_AREA)
    # Vom returna imaginea astfel obtinuta
    return image