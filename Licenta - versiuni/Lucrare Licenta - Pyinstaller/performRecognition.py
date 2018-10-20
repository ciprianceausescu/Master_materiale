# Importarea librariilor necesare
import cv2
from sklearn.externals import joblib
from skimage.feature import hog
import numpy as np
# Functia predictNumber primeste ca parametru numele unei imagini si ofera ca raspuns digit-ul detectat din imaginea
# primita
def predictNumber(imageName):
    # Se incarca clasificatorul
    clf = joblib.load("digitsClassifier.pkl")
    # Se citeste imaginea digitului care se doreste a fi clasificat
    image = cv2.imread(imageName)
    # Se transforma imaginea din imagine RGB in imagine in tonuri de gri
    imageGray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    # Se aplica un filtru de dilatare imaginii, de dimensiune 3x3
    roi = cv2.dilate(imageGray, (3, 3))
    # Se calculeaza histograma de gradienti orientati pentru imaginea citita
    roi_hog_fd = hog(roi, orientations=9, pixels_per_cell=(7, 7), cells_per_block=(2, 2), visualise=False)
    # Se determina numarul din imagine, folosind functia predict
    numberPredicted = clf.predict(np.array([roi_hog_fd], 'float64'))
    # Se returneaza numarul detectat
    return numberPredicted[0]