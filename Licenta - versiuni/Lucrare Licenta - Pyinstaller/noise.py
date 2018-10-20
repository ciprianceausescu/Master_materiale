# Importarea librariilor necesare
from PIL import Image, ImageFilter
# Functiile prepareImage, removeNoise, removeNoiseByPixel sunt folosite de catre functia principala a acesui modul pe
# care l-am definit, si anume, removeNoiseMain, pentru a scoate zgomotul dintr-o anumita grila selectata de catre utilizator.
# Se elimina pixelii a caror valoare este mai mica valoarea parametrului pass_factor. Acest mod de eliminare a zgomotului
# este unul foarte costisitor, avand o complexitate de O(N*M), unde N este numarul de coloane, iar M numarul de randuri
# de pixeli din imagine (in prealabil se redimensioneaza imaginea la jumatate din valorile lui N si M pentru a se efectua
# mai putine operatii)
def prepareImage(image):
    # Imaginii primite ca parametru i se aplica un filtru de blurare, definit de metoda SMOOTH_MORE
    image = image.filter(ImageFilter.SMOOTH_MORE)
    # Imaginii blurate, folosind filtrul de blurare i se aplica din nou, acelasi filtru pentru o ajustare mai precisa
    image = image.filter(ImageFilter.SMOOTH_MORE)
    # Imaginea este salvata pentru a putea fi vizualizat raspunsul filtrelor, in imaginea outputSmooth.jpg
    image.save("images/outputSmooth.jpg")
    # image.mode reprezinta modul de definire a unei imagini. Valoarea 'L' reprezinta o imagine cu valori intre 0-255,
    # adica in tonuri de gri. Daca modul de definire al imaginii nu este in tonuri de gri, aceasta se va transforma
    # intr-o imagine specificata de parametrul metodei convert (in cazul nostru, imagine in tonuri de gri)
    if image.mode != 'L':
        image = image.convert('L')
    return image

def removeNoise(image, passFactor):
    # Se parcug toti pixelii imaginii
    for columnNumber in range(image.size[0]):
        for lineNumber in range(image.size[1]):
            # Pentru fiecare pixel al imaginii se aplica metoda removeNoiseByPixel, care primeste ca parametrii imaginea,
            # coloana si linia pixelului caruia i se va schimba intensitatea, si pragul de valoare
            value = removeNoiseByPixel(image, columnNumber, lineNumber, passFactor)
            # Valoarea determinata de metoda apelata la pasul anterior, se la atribui pixelului de pe linia si coloana
            # respectiva, folosind metoda putpixel
            image.putpixel((columnNumber, lineNumber), value)
    return image

def removeNoiseByPixel(image, columnNumber, lineNumber, passFactor):
    # Se extrage pixelul de pe linia si coloana specificata de parametrii primiti, iar daca valoarea acestuia este
    # sub valoarea lui passFactor, acesta va deveni pixel negru, iar daca valoarea este peste passFactor, va deveni
    # pixel alb
    if image.getpixel((columnNumber, lineNumber)) < passFactor:
        return (0)
    return (255)


def removeNoiseMain(imageName, passFactor):
    # Se defineste calea la care se va salva imaginea din care s-a eliminat zgomotul
    outputImage = 'images/noiseRemoved.jpg'
    # Se deschide imaginea, folosind functia open
    image = Image.open(imageName)
    # Se redimensioneaza, la jumatate din numarul de linii si coloane
    image = image.resize((int(image.size[0]/2), int(image.size[1]/2)), Image.ANTIALIAS)
    # Se aplica de rand metodele definite anterior, si anume prepareImage si removeNoise
    image = prepareImage(image)
    image = removeNoise(image, passFactor)
    # Imaginea din care s-a eliminat zgomotul este salvata in locul specificat de parametru outputImage
    image.save(outputImage)