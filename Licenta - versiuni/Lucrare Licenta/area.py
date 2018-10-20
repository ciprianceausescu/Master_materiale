# Functia area calculeaza aria unui triunghi dat de colturile sale (xA, yA), (xB, yB), (xC, yC)
# folosind formula matematica cu determinatul aria = |detTriunghi| / 2, unde detTriunghi este
# [xA, yA, 1 / xB, yB, 1 / xC, yC, 1]
def area(xA, yA, xB, yB, xC, yC):
    return abs(xA*yB + xB*yC + xC*yA - xC*yB - xA*yC - xB*yA) / 2