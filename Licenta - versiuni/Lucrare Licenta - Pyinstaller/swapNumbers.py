# Functiile swapNumberLH/HL (low-high)/(high-low) au rolul de a returna numele date ca parametru in dorinea dorita
# mic, mare, respectiv mare, mic
def swapNumbersLH(x,y):
    if x > y:
        aux = x
        x = y
        y = aux
    return x,y

def swapNumbersHL(x,y):
    if x < y:
        aux = x
        x = y
        y = aux
    return x,y

#a = 5
#b = 7
#a, b = swapNumbers(a, b) => a = 5, b = 7

#a = 9
#b = 3
#a, b = swapNumbers(a, b) => a = 3, b = 9