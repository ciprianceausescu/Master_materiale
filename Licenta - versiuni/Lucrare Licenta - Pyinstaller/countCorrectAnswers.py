# Importarea librariilor necesare
import numpy as np
# Definim mai jos functia countCorrectAnswers care primeste ca parametru doua matrici
# cu raspunsuri si verifica cate raspunsuri se potrivesc in cele doua matrici
def countCorrectAnswers(answersMatrix1, answersMatrix2):
    count = 0
    # Se parcurge fiecare linie din cele doua matrici, si se verifica ca vectorul asociat
    # acelei linii sa fie identic in ambele matrici date ca parametru, i.e raspunsul corect
    # se afla pe aceeasi pozitie in ambii vectori de pe linia i. Rezultatul final
    # cu numarul de raspunsuri identice in cele doua matrici se va returna prin variabila
    # count
    for i in range (0,len(answersMatrix1)):
        if np.array_equal(answersMatrix1[i],answersMatrix2[i]):
            count = count + 1
    return count