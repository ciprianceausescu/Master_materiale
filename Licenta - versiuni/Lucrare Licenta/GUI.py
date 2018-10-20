# Importarea librariilor necesare
from countCorrectAnswers import countCorrectAnswers
from tkinter import Tk, Label, Button, StringVar, messagebox, Entry, OptionMenu, Toplevel, filedialog, PhotoImage
from finalMark import finalMark
import os
from unitTest import unitTest
from datetime import datetime
from natsort import natsorted
from sklearn.metrics import accuracy_score
from answersImage import *
from evaluateDirectory import evaluateDirectory
# Clasa pentru interfata grafica a aplicatiei
class GUI:
    def __init__(self, master):
        # Functia init are rolul de a initializa interfata cu utilizatorul - in sensul ca aceasta defineste variabilele M_1 - M_4,
        # I_1 - I_4, F_1 - F_4 in care vor fi stocate matricile cu raspunsurile corespunzatoare acelor grile (M_1 = matematica 1,
        # I_1 = informatica 1, F_1 = fizica 1)
        self.master = master
        master.title("Admitere CTI")

        rowNum = 15
        colNum = 4
        self.p1 = 0
        self.p2 = 0
        self.e1 = 0
        self.e2 = 0
        # Se definesc matricile de raspunsuri M_1 - M_4, I_1 - I_4, F_1 - F_4
        # Aceste matrici vor primi valori implicit, corespunzatoare cu baremele admiterii din vara anului 2016
        self.M_1 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.M_1 = [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0],
                    [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0]]
        self.M_2 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.M_2 = [[0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0],
                    [0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0]]
        self.M_3 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.M_3 = [[0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1],
                    [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1]]
        self.M_4 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.M_4 = [[0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0],
                    [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0]]

        self.I_1 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.I_1 = [[0, 0, 1, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0],
                    [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0]]
        self.I_2 = np.zeros((rowNum, colNum), dtype=np.uint8)
        self.I_2 = [[1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1],
                    [1, 0, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1]]
        self.I_3 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.I_3 = [[0, 1, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1],
                    [0, 0, 1, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]
        self.I_4 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.I_4 = [[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0],
                    [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0]]

        self.F_1 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.F_1 = [[0, 1, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 1, 0],
                    [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0]]
        self.F_2 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.F_2 = [[0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0],
                    [0, 0, 0, 1], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]
        self.F_3 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.F_3 = [[0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1],
                    [1, 0, 0, 0], [1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0]]
        self.F_4 = np.zeros((rowNum, colNum),dtype=np.uint8)
        self.F_4 = [[1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1],
                    [0, 0, 0, 1], [0, 1, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 0]]
        # Variabila type va avea valoarea grilei completate de elev, si anume Informatica sau Fizica
        self.type = ""
        # guiImage reprezinta logo-ul aplicatiei (sigla Facultatii de Matematica si Informatica)
        self.guiImage = PhotoImage(file="gui/FMI.png")
        # Imaginea va fi adaugata intr-un label
        self.imglabel = Label(master, image=self.guiImage)
        # Label-ul asociat imaginii se va adauga in interfata grafica la pozitiile row si column specificate
        self.imglabel.grid(row=0, column=1, padx=30, pady=10)

        # Variabila teacherNameValue reprezinta valoarea campului de intrare asociat profesorului care utilizeaza aplicatia
        # la momentul respectiv
        teacherNameValue = StringVar(master, value="prof dr. Ion Ion")
        # teacherName este campul de intrare asociat profesorului care utilizeaza aplicatia la momentul respectiv
        self.teacherName = Entry(master, width=30, textvariable=teacherNameValue, justify='center')
        # Campul de intrare asociat profesorului se va adauga in interfata grafica la pozitiile row si column specificate
        self.teacherName.grid(row=1, column=0, padx=20, pady=10)

        # Butonul openP are rolul de a deschide imaginea grilei corecte (aceea a profesorului), insa
        # momentan nu se mai utilizeaza aceasta abordare (la momentul apasarii butonul se va apela functia openP)
        self.openP = Button(master, text="Evalueaza grila profesor", command=self.openTeacher, width = 20, height = 2)
        # Butonul openP se va adauga in interfata grafica la pozitiile row si column specificate
        self.openP.grid(row=2, column=0, padx=20, pady=10)

        # Labelul label1 este folosit pentru a afisa calea corespunzatoare a imaginii selectate de butonul openP,
        # insa momentan nu vom mai folosi acest label
        self.labelText1 = StringVar()
        self.labelText1.set("")
        self.label1 = Label(master, textvariable=self.labelText1)
        self.label1.grid(row=2, column=1, padx=20, pady=10)

        # Variabila passFactor reprezinta valoarea initiala a campului de intrare asociat parametrului cu care se va efectua
        # eliminarea zgomotului din lucrarile corectate
        passFactor = StringVar(master, value="170") # Introduceti passFactor (0-255)
        # passFactor este campul de intrare asociat parametrului cu care se va efectua
        # eliminarea zgomotului din lucrarile corectate
        self.passFactor = Entry(master, width=30, textvariable=passFactor, justify='center')
        # Campul de intrare asociat parametrului de eliminare a zgomotului se va adauga in interfata grafica la
        # pozitiile row si column specificate
        self.passFactor.grid(row=3, column=0, padx=20, pady=10)

        # Butonul openE are rolul de a deschide imaginea grilei elevului si evaluarii acesteia (la momentul apasarii
        # butonului se va apela functia openE)
        self.openE = Button(master, text="Evalueaza grila elev", command=self.openStudent, width = 20, height = 2)
        # Butonul openE se va adauga in interfata grafica la pozitiile row si column specificate
        self.openE.grid(row=4, column=0, padx=20, pady=10)

        # Labelul label2 este folosit pentru a afisa calea corespunzatoare a imaginii selectate de butonul openE,
        # insa momentan nu vom mai folosi acest label
        self.labelText2 = StringVar()
        self.labelText2.set("")
        self.label2 = Label(master, textvariable=self.labelText2)
        self.label2.grid(row=4, column=1, padx=20, pady=10)

        # Butonul evaluateDirectory va deschide o fereastra pentru evaluarea lucrarilor dintr-un folder
        self.evaluateDirectory = Button(master, text="Ev grila elev (Tip selectat)", command=self.evaluateDirectory,
                                        width=20, height=2)
        # Butonul evaluareDirectory se va adauga in interfata grafica la pozitiile row si column specificate
        self.evaluateDirectory.grid(row=5, column=0, padx=20, pady=10)

        # Butonul runTests va rula testele unitare create pentru aplicatie
        self.runTests = Button(master, text="Ruleaza testele", command=self.runTests, width=20, height=2)
        # Butonul runTests se va adauga in interfata grafica la pozitiile row si column specificate
        self.runTests.grid(row=6, column=0, padx=20, pady=10)

        # Butonul addMatrix va deschide o noua fereastra care are scopul de a introduce valorile elementelor matricilor
        # de raspunsuri corecte oferite de catre comisia de admitere
        self.addMatrix = Button(root, text="Adauga raspunsuri", command=self.createNewWindow, width=20, height=2)
        # Butonul addMatrix se va adauga in interfata grafica la pozitiile row si column specificate
        self.addMatrix.grid(row=7, column=0, padx=20, pady=10)

        # Butonul runTests va rula testele unitare create pentru aplicatie
        self.answersImage = Button(master, text="Afiseaza lucrare corectata", command=self.answersImage, width=20, height=2)
        # Butonul runTests se va adauga in interfata grafica la pozitiile row si column specificate
        self.answersImage.grid(row=8, column=0, padx=20, pady=10)

        # Butonul closeButton va inchide fereastra gui-ului
        self.closeButton = Button(master, text="Inchide", command=master.quit, width = 20, height = 2)
        # Butonul closeButton se va adauga in interfata grafica la pozitiile row si column specificate
        self.closeButton.grid(row=9, column=0, padx=20, pady=10)

    def createNewWindow(self):
        # Functia createNewWindow are rolul de crea text box-urile in care vor fi introduse valorile corecte
        # pentru fiecare grila (grila corespunzatoare drop down-ului menu - M_1, M_2 etc)
        # Fiecare text box are o valoare initiala egala cu 0 si va fi pozitionat in interfata grafica la
        # pozitiile row si column specificate
        self.top = Toplevel()
        self.top.title("Adaugare raspunsuri corecte")

        label1 = StringVar(self.top, value="1")
        self.lab1 = Label(self.top, width=5, textvariable=label1, justify='center')
        self.lab1.grid(row=1, column=0, pady=3)

        label2 = StringVar(self.top, value="2")
        self.lab2 = Label(self.top, width=5, textvariable=label2, justify='center')
        self.lab2.grid(row=2, column=0, pady=3)

        label3 = StringVar(self.top, value="3")
        self.lab3 = Label(self.top, width=5, textvariable=label3, justify='center')
        self.lab3.grid(row=3, column=0, pady=3)

        label4 = StringVar(self.top, value="4")
        self.lab4 = Label(self.top, width=5, textvariable=label4, justify='center')
        self.lab4.grid(row=4, column=0, pady=3)

        label5 = StringVar(self.top, value="5")
        self.lab5 = Label(self.top, width=5, textvariable=label5, justify='center')
        self.lab5.grid(row=5, column=0, pady=3)

        label6 = StringVar(self.top, value="6")
        self.lab6 = Label(self.top, width=5, textvariable=label6, justify='center')
        self.lab6.grid(row=6, column=0, pady=3)

        label7 = StringVar(self.top, value="7")
        self.lab7 = Label(self.top, width=5, textvariable=label7, justify='center')
        self.lab7.grid(row=7, column=0, pady=3)

        label8 = StringVar(self.top, value="8")
        self.lab8 = Label(self.top, width=5, textvariable=label8, justify='center')
        self.lab8.grid(row=8, column=0, pady=3)

        label9 = StringVar(self.top, value="9")
        self.lab9 = Label(self.top, width=5, textvariable=label9, justify='center')
        self.lab9.grid(row=9, column=0, pady=3)

        label10 = StringVar(self.top, value="10")
        self.lab10 = Label(self.top, width=5, textvariable=label10, justify='center')
        self.lab10.grid(row=10, column=0, pady=3)

        label11 = StringVar(self.top, value="11")
        self.lab11 = Label(self.top, width=5, textvariable=label11, justify='center')
        self.lab11.grid(row=11, column=0, pady=3)

        label12 = StringVar(self.top, value="12")
        self.lab12 = Label(self.top, width=5, textvariable=label12, justify='center')
        self.lab12.grid(row=12, column=0, pady=3)

        label13 = StringVar(self.top, value="13")
        self.lab13 = Label(self.top, width=5, textvariable=label13, justify='center')
        self.lab13.grid(row=13, column=0, pady=3)

        label14 = StringVar(self.top, value="14")
        self.lab14 = Label(self.top, width=5, textvariable=label14, justify='center')
        self.lab14.grid(row=14, column=0, pady=3)

        label15 = StringVar(self.top, value="15")
        self.lab15 = Label(self.top, width=5, textvariable=label15, justify='center')
        self.lab15.grid(row=15, column=0, pady=3)

        label16 = StringVar(self.top, value="A")
        self.lab16 = Label(self.top, width=5, textvariable=label16, justify='center')
        self.lab16.grid(row=0, column=1, pady=3)

        label17 = StringVar(self.top, value="B")
        self.lab17 = Label(self.top, width=5, textvariable=label17, justify='center')
        self.lab17.grid(row=0, column=2, pady=3)

        label18 = StringVar(self.top, value="C")
        self.lab18 = Label(self.top, width=5, textvariable=label18, justify='center')
        self.lab18.grid(row=0, column=3, pady=3)

        label19 = StringVar(self.top, value="D")
        self.lab19 = Label(self.top, width=5, textvariable=label19, justify='center')
        self.lab19.grid(row=0, column=4, pady=3)

        self.c11 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c12 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c13 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c14 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c15 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c16 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c17 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c18 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c19 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c110 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c111 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c112 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c113 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c114 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c115 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c11.grid(row=1, column=1, pady=3)
        self.c12.grid(row=2, column=1, pady=3)
        self.c13.grid(row=3, column=1, pady=3)
        self.c14.grid(row=4, column=1, pady=3)
        self.c15.grid(row=5, column=1, pady=3)
        self.c16.grid(row=6, column=1, pady=3)
        self.c17.grid(row=7, column=1, pady=3)
        self.c18.grid(row=8, column=1, pady=3)
        self.c19.grid(row=9, column=1, pady=3)
        self.c110.grid(row=10, column=1, pady=3)
        self.c111.grid(row=11, column=1, pady=3)
        self.c112.grid(row=12, column=1, pady=3)
        self.c113.grid(row=13, column=1, pady=3)
        self.c114.grid(row=14, column=1, pady=3)
        self.c115.grid(row=15, column=1, pady=3)
        self.c21 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c22 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c23 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c24 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c25 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c26 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c27 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c28 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c29 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c210 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c211 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c212 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c213 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c214 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c215 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c21.grid(row=1, column=2, pady=3)
        self.c22.grid(row=2, column=2, pady=3)
        self.c23.grid(row=3, column=2, pady=3)
        self.c24.grid(row=4, column=2, pady=3)
        self.c25.grid(row=5, column=2, pady=3)
        self.c26.grid(row=6, column=2, pady=3)
        self.c27.grid(row=7, column=2, pady=3)
        self.c28.grid(row=8, column=2, pady=3)
        self.c29.grid(row=9, column=2, pady=3)
        self.c210.grid(row=10, column=2, pady=3)
        self.c211.grid(row=11, column=2, pady=3)
        self.c212.grid(row=12, column=2, pady=3)
        self.c213.grid(row=13, column=2, pady=3)
        self.c214.grid(row=14, column=2, pady=3)
        self.c215.grid(row=15, column=2, pady=3)
        self.c31 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c32 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c33 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c34 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c35 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c36 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c37 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c38 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c39 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c310 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c311 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c312 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c313 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c314 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c315 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c31.grid(row=1, column=3, pady=3)
        self.c32.grid(row=2, column=3, pady=3)
        self.c33.grid(row=3, column=3, pady=3)
        self.c34.grid(row=4, column=3, pady=3)
        self.c35.grid(row=5, column=3, pady=3)
        self.c36.grid(row=6, column=3, pady=3)
        self.c37.grid(row=7, column=3, pady=3)
        self.c38.grid(row=8, column=3, pady=3)
        self.c39.grid(row=9, column=3, pady=3)
        self.c310.grid(row=10, column=3, pady=3)
        self.c311.grid(row=11, column=3, pady=3)
        self.c312.grid(row=12, column=3, pady=3)
        self.c313.grid(row=13, column=3, pady=3)
        self.c314.grid(row=14, column=3, pady=3)
        self.c315.grid(row=15, column=3, pady=3)
        self.c41 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c42 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c43 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c44 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c45 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c46 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c47 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c48 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c49 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c410 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c411 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c412 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c413 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c414 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c415 = Entry(self.top, width=5, textvariable = StringVar(self.top, value='0'))
        self.c41.grid(row=1, column=4, pady=3)
        self.c42.grid(row=2, column=4, pady=3)
        self.c43.grid(row=3, column=4, pady=3)
        self.c44.grid(row=4, column=4, pady=3)
        self.c45.grid(row=5, column=4, pady=3)
        self.c46.grid(row=6, column=4, pady=3)
        self.c47.grid(row=7, column=4, pady=3)
        self.c48.grid(row=8, column=4, pady=3)
        self.c49.grid(row=9, column=4, pady=3)
        self.c410.grid(row=10, column=4, pady=3)
        self.c411.grid(row=11, column=4, pady=3)
        self.c412.grid(row=12, column=4, pady=3)
        self.c413.grid(row=13, column=4, pady=3)
        self.c414.grid(row=14, column=4, pady=3)
        self.c415.grid(row=15, column=4, pady=3)

        # menu este drop down-ul creat a carui valoare la momentul deschiderii ferestrei va fi mereu M_1, iar valorile
        # posibile ale acestui drop down vor fi date ca parametru in momentul definirii acestuia
        value = StringVar(self.top)
        value.set("Matematica_1")  # Valoarea initiala a drop down-ului
        self.menu = OptionMenu(self.top, value, "Matematica_1", "Matematica_2", "Matematica_3", "Matematica_4",
                               "Informatica_1", "Informatica_2", "Informatica_3", "Informatica_4",
                               "Fizica_1", "Fizica_2", "Fizica_3", "Fizica_4")
        # Drop down-unul menu se va adauga in interfata grafica la pozitiile row si column specificate
        self.menu.grid(row=15, column=5, pady=3)
        # Butonul saveMatrix apeleaza functia saveMatrix care va asocia valorile introduse in text box-urile ferestrei,
        # elementelor matricei corespunzatoare valorii drop down-ului
        self.saveMatrix = Button(self.top, text="Salveaza raspunsuri", command=self.saveMatrix, width=20)
        # Butonul saveMatrix se va adauga in interfata grafica la pozitiile row si column specificate
        self.saveMatrix.grid(row=16, column=5, padx=20, pady=10)

        # Butonul closeButton va inchide fereastra deschisa
        self.closeButton = Button(self.top, text="Inchide", command=self.quitWindow, width=20)
        # Butonul closeButton se va adauga in interfata grafica la pozitiile row si column specificate
        self.closeButton.grid(row=17, column=5, padx=20, pady=10)

    def openTeacher(self):
        # Metoda care se apeleaza in momentul in care se apasa pe butonul openP
        messagebox.showinfo("Eroare", "Aceasta functionalitate nu este activa momentan.")

    def openStudent(self):
        messagebox.showinfo("Eroare", "Aceasta functionalitate nu este activa momentan.")
        return
        # Se verifica daca s-a introdus o valoare pentru campul passFactor si daca valoarea sa este intre 0 si 255
        try:
            passFactor = int(self.passFactor.get())
            if (passFactor < 0 or passFactor > 255):
                messagebox.showinfo("Eroare", "Trebuie sa adaugati o valoare pentru campul passFactor intre 0 si 255.")
                return
        except:
            messagebox.showinfo("Eroare", "Trebuie sa adaugati o valoare pentru campul passFactor.")
            return

        # Metoda care se apeleaza in momentul in care se apasa pe butonul openE

        # Vectorul resultTestArray este format din 191 de digiti, care apar in cele 191 de lucrari
        # pe care le-am folosit in testarea aplicatiei
        resultTestArray = [3,2,2,1,1,2,4,3,1,3,3,4,1,2,3,4,2,3,4,4,2,3,2,1,4,1,3,4,3,
                           1,3,3,4,1,3,2,1,4,4,1,1,2,4,2,4,1,1,3,2,4,3,1,2,1,3,1,3,3,
                           3,4,1,1,2,4,4,1,1,4,2,3,3,4,1,3,2,1,3,1,4,4,1,1,4,1,4,3,3,
                           1,1,3,4,1,2,1,1,1,3,1,1,3,2,1,4,3,2,1,4,2,2,2,2,4,2,3,4,2,
                           4,4,4,3,4,3,3,1,1,3,4,4,1,4,3,3,1,1,2,2,2,2,4,1,2,1,3,3,1,
                           1,1,4,3,3,3,3,3,1,1,3,4,4,3,1,2,2,3,4,2,3,1,2,2,1,2,3,2,3,
                           1,4,1,2,4,4,2,1,3,1,2,4,2,2,2,1,1]
        # In vectorul resultPredictArray o sa adaugam de fiecare data digit-ul recunoscut folosind metoda append
        resultPredictArray = []
        # Variabila teacherNameValue va lua valoarea campului de intrare teacherName
        teacherNameValue = self.teacherName.get()
        # Lista imageNotUsed reprezinta lista numelor imaginilor care rezulta in urma procesarii lucrarilor
        # si care nu trebuie luate in calcul in momentul corectarii automate
        imageNotUsed = ['grila1.jpg', 'grila2.jpg', 'grilaDetectat1.jpg', 'grilaDetectat2.jpg', 'output.jpg', 'noiseRemoved.jpg',
                   'outputSmooth.jpg', 'grilaCorectat1.jpg', 'grilaCorectat2.jpg', 'imgType.jpg', 'digit.jpg']
        self.dir_opt = options = {}
        options['initialdir'] = os.getcwd()
        options['mustexist'] = False
        options['parent'] = root
        options['title'] = 'Selectati directorul cu testele pentru corectat'
        try:
            # Variabilele N1 si N2 reprezinta notele pentru cele doua grile din lucrare
            N1 = 0  # nota grilei 1
            N2 = 0  # nota grilei 2
            # In variabila directory se va salva numele directorului ales de catre utilizator, in care se
            # gasesc lucrarile de corectat
            directory = filedialog.askdirectory(**self.dir_opt)
            # imageNameList va cuprinde numele tuturor fisierelor existente in directorul selectat de catre utilizator
            imageNameList = os.listdir(directory)
            # Lista va fi sortata astfel incat la final aceasta sa cuprinda numele lucrarilor care se corecteaza
            # in ordinea in care se gasesc in director (aranjate alfabetic dupa nume)
            imageNameList = natsorted(imageNameList, reverse = False)
            # Variabila file va reprezenta fisierul in care se vor scrie rezultatele dupa ce se corecteaza lucrarile
            file = open("Rezultate/Rezultate.txt", 'w')
            # Daca fisierul exista, se va sterge tot continutul sau
            file.truncate()
            # Se va scrie un antent corespunzator: Profesorul care a utilizat aplicatia si ora la care a pornit corectura
            file.write("Profesor comisie: " + teacherNameValue + "\n" + "Teste evaluate la: " + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + "\n")
            file.write("_____________________________________________________________________________________\n")
            # Se va itera prin toate imaginile din directorul ales
            for i in range(0, len(imageNameList)):
                # Se vor alege doar acele imagini al caror nume nu se afla in lista de imagini imageNotUsed
                if (imageNameList[i] not in imageNotUsed):
                    print(imageNameList[i])
                    # Se elimina zgomotul din imaginea de la pasul curent
                    removeNoiseMain('images/' + imageNameList[i], passFactor)
                    # rotim imaginea din care am eliminat zgomototul, salvand cooronatele grilelor (1 si 2)
                    (x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6, x7, y7, x8, y8, rotateNumber) = rotateImage('images/noiseRemoved.jpg')
                    # Citim imaginea initiala
                    imageInitial = cv2.imread('images/' + imageNameList[i])
                    # Se iau dimensiunile imaginii initiale, pe care le vom folosi la apelul metodei findTestType2
                    self.imageInitialWidth = imageInitial.shape[1]
                    self.imageInitialHeight = imageInitial.shape[0]
                    # Redimensionam imaginea initiala
                    imageInitial = cv2.resize(imageInitial, (620, 870), interpolation=cv2.INTER_AREA)
                    # Rotim imaginea in functie de parametrul rotNum returnat mai sus de functia rotateImage
                    rowNum, colNum, _ = imageInitial.shape
                    rotateMatrix = cv2.getRotationMatrix2D((colNum / 2, rowNum / 2), rotateNumber, 1)
                    imgInitialRot = cv2.warpAffine(imageInitial, rotateMatrix, (colNum, rowNum))
                    # Decupam grilele din imaginea initiala
                    grille1 = imgInitialRot[y1:y3, x1:x3]
                    grille2 = imgInitialRot[y5:y7, x5:x7]
                    # Salvam cele doua grile pe care le-am decupat
                    cv2.imwrite("images/grilaDetectat1.jpg", grille1)
                    cv2.imwrite("images/grilaDetectat2.jpg", grille2)
                    # Determinam matricile de raspunsuri pentru cele doua grile determinate in grilaDetectat1 si grilaDetectat2
                    self.e1 = resultMatrix('images/grilaDetectat1.jpg', 15, 4, 2)
                    self.e2 = resultMatrix('images/grilaDetectat2.jpg', 15, 4, 2)
                    # Determinam regiunea unde se gasesc cele doua casute pentru Informatica / Fizica
                    findTestType2('images/noiseRemoved.jpg', 'images/' + imageNameList[i], self.imageInitialWidth, self.imageInitialHeight)
                    # Se determina tipul grilei Informatica sau Fizica
                    self.type = findTestType('images/noiseRemoved.jpg')
                    # Decupam digit-ul din regiunea de sus sau de jos, in functie de grila la care raspunde elevul Informatica/Fizica,
                    # pe care il vom salva ulterior in digit.jpg
                    cD.cropDigit("images/imgType.jpg")
                    # Vom determina valoarea digitului, ruland functia main din predict asupra imaginii digit.jpg
                    self.predictedDigit = predictNumber("images/digit.jpg")

                    resultPredictArray.append(self.predictedDigit)

                    # In N1 si N2 calculam notele corespunzatoare grilei de matematica, respectiv grilei de info/fizica
                    # completate de catre elev. Momentan grila se evalueaza in comparatie cu o valoarea unei grile hardcodate,
                    # urmand ca in varianta finala a aplicatiei sa se determine tipul grilei completate, iar evaluarea sa se
                    # faca in concordanta cu tipul grilei alese de catre elev
                    if (self.type == 'Informatica' and self.predictedDigit == 1):
                        N1 = countCorrectAnswers(self.M_1, self.e1)
                        N2 = countCorrectAnswers(self.I_1, self.e2)
                    if (self.type == 'Informatica' and self.predictedDigit == 2):
                        N1 = countCorrectAnswers(self.M_2, self.e1)
                        N2 = countCorrectAnswers(self.I_2, self.e2)
                    if (self.type == 'Informatica' and self.predictedDigit == 3):
                        N1 = countCorrectAnswers(self.M_3, self.e1)
                        N2 = countCorrectAnswers(self.I_3, self.e2)
                    if (self.type == 'Informatica' and self.predictedDigit == 4):
                        N1 = countCorrectAnswers(self.M_4, self.e1)
                        N2 = countCorrectAnswers(self.I_4, self.e2)
                    if (self.type == 'Fizica' and self.predictedDigit == 1):
                        N1 = countCorrectAnswers(self.M_1, self.e1)
                        N2 = countCorrectAnswers(self.F_1, self.e2)
                    if (self.type == 'Fizica' and self.predictedDigit == 2):
                        N1 = countCorrectAnswers(self.M_2, self.e1)
                        N2 = countCorrectAnswers(self.F_2, self.e2)
                    if (self.type == 'Fizica' and self.predictedDigit == 3):
                        N1 = countCorrectAnswers(self.M_3, self.e1)
                        N2 = countCorrectAnswers(self.F_3, self.e2)
                    if (self.type == 'Fizica' and self.predictedDigit == 4):
                        N1 = countCorrectAnswers(self.M_4, self.e1)
                        N2 = countCorrectAnswers(self.F_4, self.e2)

                    # Rezultatul se va scrie in fisier sub forma: imageName, Grila: tipGrila (ex: Fizica 1), numar raspunsuri
                    # matematica, numar raspunsuri informatica/fizica, nota finala
                    resultTextFile = imageNameList[i] + " Grila: " + self.type + " " + str(self.predictedDigit) + \
                                     ", [" + str(N1) + " Matematica], [" + str(N2) + " " + self.type + "]. Nota finala: " \
                                     + finalMark(N1 + N2) + "\n"
                    file.write(resultTextFile)

            # Partea de cod care urmeaza afiseaza precizia modelului creat, care este reprezentat de valoarea numarului
            # de digiti detectati corect / numarul total de digiti care ar fi trebui sa fie detectati
            if len(resultTestArray) == len(resultPredictArray):
                print("Precizie model: " + str(round(accuracy_score(resultTestArray, resultPredictArray) * 100, 2)))

            messagebox.showinfo("Informare", "Testele au fost corectate cu succes.")
            file.write("_____________________________________________________________________________________")
        except:
            messagebox.showinfo("Eroare", "Trebuie sa alegeti directorul cu testele.")
        cv2.destroyAllWindows()
    def runTests(self):
        messagebox.showinfo("Eroare", "Aceasta functionalitate nu este activa momentan.")
        return
        # Metoda care porneste rularea testelor unitare
        runTests = unitTest()
        runTests.tests()
        messagebox.showinfo("Informare", "Testele au rulat cu succes.")

    def saveMatrix(self):
        # Valoarea lui menu se ia cu cget("text")
        # Functia saveMatrix va crea matricea cu numele matrix corespunzatoare raspunsurilor pe care profesorul
        # le-a introdus in text box-urile ferestrei
        option = self.menu.cget("text")
        matrix = [[(int)(self.c11.get()),(int)(self.c21.get()),(int)(self.c31.get()),(int)(self.c41.get())],
                  [(int)(self.c12.get()), (int)(self.c22.get()), (int)(self.c32.get()), (int)(self.c42.get())],
                  [(int)(self.c13.get()), (int)(self.c23.get()), (int)(self.c33.get()), (int)(self.c43.get())],
                  [(int)(self.c14.get()), (int)(self.c24.get()), (int)(self.c34.get()), (int)(self.c44.get())],
                  [(int)(self.c15.get()), (int)(self.c25.get()), (int)(self.c35.get()), (int)(self.c45.get())],
                  [(int)(self.c16.get()), (int)(self.c26.get()), (int)(self.c36.get()), (int)(self.c46.get())],
                  [(int)(self.c17.get()), (int)(self.c27.get()), (int)(self.c37.get()), (int)(self.c47.get())],
                  [(int)(self.c18.get()), (int)(self.c28.get()), (int)(self.c38.get()), (int)(self.c48.get())],
                  [(int)(self.c19.get()), (int)(self.c29.get()), (int)(self.c39.get()), (int)(self.c49.get())],
                  [(int)(self.c110.get()), (int)(self.c210.get()), (int)(self.c310.get()), (int)(self.c410.get())],
                  [(int)(self.c111.get()), (int)(self.c211.get()), (int)(self.c311.get()), (int)(self.c411.get())],
                  [(int)(self.c112.get()), (int)(self.c212.get()), (int)(self.c312.get()), (int)(self.c412.get())],
                  [(int)(self.c113.get()), (int)(self.c213.get()), (int)(self.c313.get()), (int)(self.c413.get())],
                  [(int)(self.c114.get()), (int)(self.c214.get()), (int)(self.c314.get()), (int)(self.c414.get())],
                  [(int)(self.c115.get()), (int)(self.c215.get()), (int)(self.c315.get()), (int)(self.c415.get())],
                  ]
        # In functie de valoarea care se gaseste in drop down, se va salva matricea astfel obtinuta in
        # matricea corespunzatoare optiunii alese de catre profesor
        if option == "Matematica_1":
            self.M_1 = matrix
            messagebox.showinfo("Matrice introdusa", "Matematica varianta 1")

        if option == "Matematica_2":
            self.M_2 = matrix
            messagebox.showinfo("Matrice introdusa", "Matematica varianta 2")

        if option == "Matematica_3":
            self.M_3 = matrix
            messagebox.showinfo("Matrice introdusa", "Matematica varianta 3")

        if option == "Matematica_4":
            self.M_4 = matrix
            messagebox.showinfo("Matrice introdusa", "Matematica varianta 4")

        if option == "Informatica_1":
            self.I_1 = matrix
            messagebox.showinfo("Matrice introdusa", "Informatica varianta 1")

        if option == "Informatica_2":
            self.I_2 = matrix
            messagebox.showinfo("Matrice introdusa", "Informatica varianta 2")

        if option == "Informatica_3":
            self.I_3 = matrix
            messagebox.showinfo("Matrice introdusa", "Informatica varianta 3")

        if option == "Informatica_4":
            self.I_4 = matrix
            messagebox.showinfo("Matrice introdusa", "Informatica varianta 4")

        if option == "Fizica_1":
            self.F_1 = matrix
            messagebox.showinfo("Matrice introdusa", "Fizica varianta 1")

        if option == "Fizica_2":
            self.F_2 = matrix
            messagebox.showinfo("Matrice introdusa", "Fizica varianta 2")

        if option == "Fizica_3":
            self.F_3 = matrix
            messagebox.showinfo("Matrice introdusa", "Fizica varianta 3")

        if option == "Fizica_4":
            self.F_4 = matrix
            messagebox.showinfo("Matrice introdusa", "Fizica varianta 4")
        print(matrix)

    def answersImage(self):
        messagebox.showinfo("Eroare", "Aceasta functionalitate nu este activa momentan.")
        return
        # Metoda care porneste corectarea unei lucrari si afisarea raspunsurilor corecte in cadrul acesteia
        try:
            # Se verifica daca s-a introdus o valoare pentru campul passFactor si daca valoarea sa este intre 0 si 255
            try:
                # Se salveaza valoarea campului de intrare passFactor
                passFactor = int(self.passFactor.get())
                if (passFactor < 0 or passFactor > 255):
                    messagebox.showinfo("Eroare", "Trebuie sa adaugati o valoare pentru campul passFactor intre 0 si 255.")
                    return
            except:
                messagebox.showinfo("Eroare", "Trebuie sa adaugati o valoare pentru campul passFactor.")
                return
            # Se alege lucrarea pentru care se doreste afisarea raspunsurilor
            fileName = filedialog.askopenfilename()
            # Se apeleaza metoda answersImage pentru imaginea selectata
            answersImage(fileName, passFactor, self.M_1, self.M_2, self.M_3, self.M_4, self.I_1, self.I_2, self.I_3, self.I_4, self.F_1,
                         self.F_2, self.F_3, self.F_4)
        except:
            messagebox.showinfo("Eroare", "Trebuie sa alegeti o lucrare pentru a fi corectata.")

    def evaluateDirectory(self):
        # Metoda corecteaza imaginile dintr-un folder selectat
        self.top = Toplevel()
        self.top.title("Evalueaza folder")
        self.top.geometry("300x150")
        # Drop down-ul directoryMenu ofera posibilitatea de a selecta tipul grilelor existente in folderul care se va
        # select de catre utilizator
        value = StringVar(self.top)
        value.set("Informatica_1")  # Valoarea initiala a drop down-ului
        # Valorile drop down-ului
        self.directoryMenu = OptionMenu(self.top, value, "Informatica_1", "Informatica_2", "Informatica_3", "Informatica_4",
                               "Fizica_1", "Fizica_2", "Fizica_3", "Fizica_4")
        # Drop down-ul se adauga in interfata grafica la pozitiile row si column specificate
        self.directoryMenu.grid(row=0, column=0, pady=3)

        # Butonul selectDirectory va oferi posibilitatea de a selecta folderul din care vom extrage imaginile pe care le vom
        # corecta
        self.selectDirectory = Button(self.top, text="Selecteaza folder", command=self.selectDirectory, width=20)
        # Butonul selectDirectory se va adauga in interfata grafica la pozitiile row si column specificate
        self.selectDirectory.grid(row=1, column=0, padx=20, pady=10)

        # Butonul closeButton va inchide fereastra deschisa
        self.closeButton = Button(self.top, text="Inchide", command=self.quitWindow, width=20)
        # Butonul closeButton se va adauga in interfata grafica la pozitiile row si column specificate
        self.closeButton.grid(row=2, column=0, padx=20, pady=10)

    def selectDirectory(self):
        # Metoda de mai jos va initializa variabila option cu o valoare determinata in functie de valoarea drop down-ului
        # corespunzator si va apela functia evaluateDirectory
        option = self.directoryMenu.cget("text")
        # Daca optiunea selectata de catre utilizator este Informatica_1, grilleType va deveni I_1
        if option == "Informatica_1":
            self.grilleType = "I_1"
        if option == "Informatica_2":
            self.grilleType = "I_2"
        if option == "Informatica_3":
            self.grilleType = "I_3"
        if option == "Informatica_4":
            self.grilleType = "I_4"
        if option == "Fizica_1":
            self.grilleType = "F_1"
        if option == "Fizica_2":
            self.grilleType = "F_2"
        if option == "Fizica_3":
            self.grilleType = "F_3"
        if option == "Fizica_4":
            self.grilleType = "F_4"
        try:
            directoryPath = filedialog.askdirectory()
            try:
                # Se salveaza valoarea campului de intrare passFactor
                passFactor = int(self.passFactor.get())
                if (passFactor < 0 or passFactor > 255):
                    messagebox.showinfo("Eroare",
                                        "Trebuie sa adaugati o valoare pentru campul passFactor intre 0 si 255.")
                    return
            except:
                messagebox.showinfo("Eroare", "Trebuie sa adaugati o valoare pentru campul passFactor.")
                return
            # Se apeleaza metoda evaluateDirectory care primeste ca parametru matricile de raspunsuri definite anterior,
            # si parametrul grilleType, avand o valoare diferita in functie de tipul lucrarilor din folderul respectiv
            self.dirName = evaluateDirectory(directoryPath, passFactor, self.M_1, self.M_2, self.M_3, self.M_4, self.I_1, self.I_2, self.I_3, self.I_4, self.F_1,
                                self.F_2, self.F_3, self.F_4, self.grilleType)
            messagebox.showinfo("Informare", "Lucrarile din folderul " + self.dirName + " au fost corectate cu succes")
            self.top.destroy()
        except:
            messagebox.showinfo("Eroare", "Trebuie sa alegeti directorul cu testele.")

    def quitWindow(self):
        # Metoda care se apeleaza in momentul in care se inchide fereastra de adaugare a raspunsurilor,
        # prin apasarea butonului Inchide
        self.top.destroy()

# Se instantiaza un obiect de tip Tkinter, a carui nume va fi Licenta si dimeniune 670x500
root = Tk()
root.title("Licenta")
root.geometry("670x660")
runGUI = GUI(root)
root.mainloop()