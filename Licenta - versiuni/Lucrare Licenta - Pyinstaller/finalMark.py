# Functia finalMark calculeaza nota finala a elevului respectiv, folosind formula numar raspunsuri * 0.3p + 1p,
# rezultatul fiind aproximat cu 2 zecimale
def finalMark(countCorrectAnswers):
    return str(round(1 + countCorrectAnswers * 0.30, 2))
