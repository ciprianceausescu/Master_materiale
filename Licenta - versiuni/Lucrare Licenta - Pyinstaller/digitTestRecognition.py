# Importarea librariilor necesare
from natsort import natsorted
from sklearn.metrics import accuracy_score
import os
from performRecognition import predictNumber
import time
# startTime = timpul la care a inceput testarea clasificatorului
startTime = time.time()
# resultLabels este vectorul care cuprinde etichetele tuturor imaginilor din setul de testare a clasificatorului
# Setul de testare cuprinde cele 191 de cifre care au fost decupate din imaginile de test (de la admiterea din iulie 2016)
resultLabels = [3,2,2,1,1,2,4,3,1,3,3,4,1,2,3,4,2,3,4,4,2,3,2,1,4,1,3,4,3,
                1,3,3,4,1,3,2,1,4,4,1,1,2,4,2,4,1,1,3,2,4,3,1,2,1,3,1,3,3,
                3,4,1,1,2,4,4,1,1,4,2,3,3,4,1,3,2,1,3,1,4,4,1,1,4,1,4,3,3,
                1,1,3,4,1,2,1,1,1,3,1,1,3,2,1,4,3,2,1,4,2,2,2,2,4,2,3,4,2,
                4,4,4,3,4,3,3,1,1,3,4,4,1,4,3,3,1,1,2,2,2,2,4,1,2,1,3,3,1,
                1,1,4,3,3,3,3,3,1,1,3,4,4,3,1,2,2,3,4,2,3,1,2,2,1,2,3,2,3,
                1,4,1,2,4,4,2,1,3,1,2,4,2,2,2,1,1]
# resultPredicted este vectorul in care se vor adauga cifrele prezise de clasificator
resultPredicted = []
# imageNameList este lista numelor tuturor imaginilor din setul de testare
imageNameList = os.listdir("digits")
# Se sorteaza lista pentru ca elementele sa fie in ordine alfabetica
imageNameList = natsorted(imageNameList, reverse = False)
# Se parcurge lista cu numele tuturor imaginilor din setul de testare
for i in range(0, len(imageNameList)):
    # Se prezice cifra din imaginea de la iteratia curenta
    numberPredited = predictNumber("digits/" + imageNameList[i])
    # Rezultatul prezis se adauga in lista de cifre detectate
    resultPredicted.append(numberPredited)
# Se va afisa precizia modelul, reprezentand raportul dintre numarul total de cifre si numarul total de cifre detectate
# corect
print("Precizie model: " + str(round(accuracy_score(resultLabels, resultPredicted) * 100, 2)))
# Se va afisa timpul total necesar testarii
print(str(len(imageNameList))+" imagini---Timpul rularii etapei de testare in secunde: %s ---" % (time.time() - startTime))