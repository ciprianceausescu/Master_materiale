# Acest clasificator in acest moment ofera un procentaj de 76.96% pentru detectia tipului de grila completata

# Importarea librariilor necesare
from sklearn.externals import joblib
from sklearn import datasets
from skimage.feature import hog
from sklearn.svm import LinearSVC
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import accuracy_score
import time
from sklearn import svm
# startTime = timpul la care a inceput antrenarea clasificatorului
startTime = time.time()
# Incarcam setul de date pentru generarea modelului
datasetMNIST = datasets.fetch_mldata("MNIST Original")
# Setul de date este impartit in imagini reprezentand digitii si un vector format din valorile digitilor
# dataset.data[0] - imaginea unui zero, dataset.target[0] = 0
imagesMNIST = np.array(datasetMNIST.data, 'int16')
valuesMNIST = np.array(datasetMNIST.target, 'int')
# Se afiseaza prima imagine din setul de date MNIST
"""
firstImage = np.array(features[0], dtype='uint8')
pixels = firstImage.reshape((28, 28))
plt.imshow(pixels, cmap='gray')
plt.waitforbuttonpress()
"""
# Se aplica metoda de histograme de gradienti orientati doar pentru imaginile care contin cifre de 1, 2, 3, 4
# hogDescriptors = va salva lista cu descriptorul fiecarei imagini pentru care a fost calculata HOG
hogDescriptors = []
# labels4Digits = va salva lista cu numarul reprezentat de fiecare imagine care cuprinde digit-ul de 1, 2, 3, 4
labels4Digits = []
for i in range(0,len(imagesMNIST)-10000):
    # Se vor lua in considerare doar imaginile dataset.data[i] care contin digiti de 1, 2, 3 sau 4, i.e a caror valoare
    # dataset.target[i] este 1, 2, 3 sau 4
    if valuesMNIST[i]==1 or valuesMNIST[i]==2 or valuesMNIST[i]==3 or valuesMNIST[i]==4:
        # Etichetele corespunzatoare acestora se vor adauga in lista labels4Digits
        labels4Digits.append(valuesMNIST[i])
        # Se va calcula descriptorul HOG pentru imaginea de la pasul curent
        fd = hog(imagesMNIST[i].reshape((28, 28)), orientations=9, pixels_per_cell=(7, 7), cells_per_block=(2, 2), visualise=False)
        # Descriptorul se va adauga in lista de descriptori finali
        hogDescriptors.append(fd)
# Lista descriptorilor se va transforma intr-un vector
hogFeatures = np.array(hogDescriptors, 'float64')
# Se creaza un clasificator liniar SVC - Support Vector Classification
clf = svm.SVC(kernel='linear')
# Se realizeaza procesul de invatare
clf.fit(hogFeatures, labels4Digits)
# Se va afisa dimensiunea setului de date de antrenare
# print(len(labels4Digits))
# Se salveaza modelul creat in digitsClassifier.pkl
joblib.dump(clf, "digitsClassifier.pkl", compress=3)
# Se va afisa timpul total necesar testarii
print(str(len(labels4Digits))+" imagini---Timpul rularii etapei de antrenare in secunde: %s ---" % (time.time() - startTime))

# startTime = timpul la care a inceput testarea clasificatorului
startTime = time.time()
# Se verifica precizia modelului
# Se declara un vector de rezultate prezise pentru ultimele 10000 de imagini din setul MNIST
predictedResult = []
# Se declara un vector de etichete pentru ultimele 10000 de imagini din setul MNIST
labelsResult = []
# Se itereaza prin ultimele 10000 de imagini
for i in range(len(imagesMNIST)-10000+1,len(imagesMNIST)):
    # Se vor lua in considerare doar imaginile dataset.data[i] care contin digiti de 1, 2, 3 sau 4, i.e a caror valoare
    # dataset.target[i] este 1, 2, 3 sau 4
    if valuesMNIST[i]==1 or valuesMNIST[i]==2 or valuesMNIST[i]==3 or valuesMNIST[i]==4:
        # Etichetele corespunzatoare acestora se vor adauga in lista labels4Digits
        labelsResult.append(valuesMNIST[i])
        # Se va calcula descriptorul HOG pentru imaginea de la pasul curent
        fd = hog(imagesMNIST[i].reshape((28, 28)), orientations=9, pixels_per_cell=(7, 7), cells_per_block=(2, 2),
                 visualise=False)
        # Se va face clasificarea imaginii de la pasul curent
        numberPredicted = clf.predict(np.array([fd], 'float64'))
        # Rezultatul clasificarii de va adauga in vectorul de rezultate prezise
        predictedResult.append(numberPredicted)
        # print("Label:", valuesMNIST[i], " predicted:", numberPredicted)
# Se va afisa dimensiunea setului de date de test
# print(len(labelsResult))
# Se va calcula procentajul determinat de modelul pe care l-am creat
print("Precizie model: " + str(round(accuracy_score(labelsResult, predictedResult) * 100, 2)))
# Se va afisa timpul total necesar testarii
print(str(len(labelsResult))+" imagini---Timpul rularii etapei de testare in secunde: %s ---" % (time.time() - startTime))