#include <iostream>
#include <cstring>

using namespace std;

static float prima;

///Clasa de baza abstracta. Nu se poate instantia, dar din ea se va mosteni
class Angajat
{
protected:
    char *nume, *prenume;
    int tip_contract, tip_activitate;
    int zi, li, ai;

public:
    Angajat(){};
    Angajat(char *numee, char *prenumee, int tc, int ta, int z, int l, int a) : tip_contract(tc), tip_activitate(ta), zi(z), li(l), ai(a)
    {
        nume = new char[strlen(numee)+1];
        strcpy(nume, numee);
        prenume = new char[strlen(prenumee)+1];
        strcpy(prenume, prenumee);
    }
    ~Angajat()
    {
        delete nume;
        delete prenume;
    }

    ///Good Practice. Altfel, daca fac o copie, ea va lucra pe aceeasi zona de memorie
    Angajat & operator = (Angajat & ang)
    {
        nume = new char[strlen(ang.nume)+1];
        strcpy(nume, ang.nume);
        prenume = new char[strlen(ang.prenume)+1];
        strcpy(prenume, ang.prenume);
        this->tip_contract = ang.tip_contract;
        this->tip_activitate = ang.tip_activitate;
        this->zi = ang.zi;
        this->li = ang.li;
        this->ai = ang.ai;
    }

    virtual void afisare() = 0;
    virtual void afisare_conditionata() = 0;
};

class Contract_permanent : public Angajat
{
public:
    Contract_permanent(){};
    Contract_permanent(char *numee, char *prenumee, int tc, int ta, int z, int l, int a) : Angajat(numee, prenumee, tc, ta, z, l, a){};
};

class TESA : public Contract_permanent
{
    int weekend;

public:
    TESA(){};
    TESA(char *numee, char *prenumee, int tc, int ta, int z, int l, int a, int week) : Contract_permanent(numee, prenumee, tc, ta, z, l, a), weekend(week){};
    void afisare()
    {
        cout<<nume<<" "<<prenume<<", contract permanent, "<<zi<<"."<<li<<"."<<ai<<", TESA, weekend "<<weekend<<", prima: "<<prima;
    }
    void afisare_conditionata()
    {
        if(weekend == 2)
            afisare();
    }
};

class Lucrativ : public Contract_permanent
{
    int nr_copii;

public:
    Lucrativ(){};
    Lucrativ(char *numee, char *prenumee, int tc, int ta, int z, int l, int a, int nr) : Contract_permanent(numee, prenumee, tc, ta, z, l, a), nr_copii(nr){};
    void afisare()
    {
        float vechime = 2014 - ai;
        float spor = (vechime/100)*prima;
        cout<<nume<<" "<<prenume<<", contract permanent, "<<zi<<"."<<li<<"."<<ai<<", lucrativ, minori "<<nr_copii<<", prima: "<<(prima+spor)*nr_copii;
    }
    void afisare_conditionata()
    {
        if(nr_copii > 0)
            afisare();
    }
};

class Contract_ora : public Angajat
{
    int zt, lt, att;

public:
    Contract_ora(){};
    Contract_ora(char *numee, char *prenumee, int tc, int ta, int z, int l, int a, int zz, int ll, int aa) : Angajat(numee, prenumee, tc, ta, z, l, a), zt(zz), lt(ll), att(aa){};
    void afisare()
    {
        cout<<nume<<" "<<prenume<<", plata cu ora, "<<zi<<"."<<li<<"."<<ai<<" - "<<zt<<"."<<lt<<"."<<att<<", prima: ";
        if(att <= 2014)
            cout<<prima/2;
        else
            cout<<prima;
    }
    void afisare_conditionata()
    {
        if((lt <= 3 && att <= 2015) || att < 2015 )
            afisare();
    }
};


int main()
{
    int nrp = 0, nr, i;
    char nume[100], prenume[100];
    int z, l, a, zt, lt, att, c, lucrator, week, copii;
    Angajat *v[2000]; ///firma nu are mai mult de 2000 de angajati

    cout<<"Care este prima fixa: ";
    cin>>prima;
    cout<<endl<<"Populam lista angajatilor firmei."<<endl;
    cout<<"Nr. angajati: "<<endl;
    cin>>nr;
    for(i = 1; i <= nr; i++)
    {
        cout<<endl<<"Angajat "<<i<<":"<<endl;
        cout<<"Nume: ";
        cin.get();
        cin.get(nume,100);
        cout<<endl<<"Prenume: ";
        cin.get();
        cin.get(prenume,100);
        cout<<endl<<"Data in care contractul a intrat in vigoare: ";
        cin>>z>>l>>a;
        cout<<endl<<"Pentru contract permanent tastati 1, pentru contract cu ora tastati 0: ";
        cin>>c;
        if(c == 1) // contract permanent
        {
            cout<<endl<<"Pentru lucrator TESA tastati 1, altfel 0: ";
            cin>>lucrator;
            if(lucrator == 1) //TESA
            {
                cout<<endl<<"In ce weekend doriti vacanta: ";
                cin>>week;
                v[++nrp] = new TESA(nume, prenume, c, lucrator, z, l, a, week);
            }
            else //lucrativ
            {
                cout<<endl<<"Nr. de copii aflati in intretinere: ";
                cin>>copii;
                v[++nrp] = new Lucrativ(nume, prenume, c, lucrator, z, l, a, copii);
            }
        }
        else //angajat cu ora
        {
            cout<<endl<<"Data terminarii contractului: ";
            cin>>zt>>lt>>att;
            v[++nrp] = new Contract_ora(nume, prenume, c, 1, z, l, a, zt, lt, att);
        }
    }

    cout<<endl<<"Selectati optiunea: \n\t1)Afisarea tuturor angajatilor \n\t2)Afisarea tuturor angajatilor lucrativi permanenti care au minori in ingrijire";
    cout<<endl<<"\t3)Afisarea tuturor angajatilor care isi fac rezervare la munte in al doilea weekend \n\t4)Afisarea tuturor angajatilor in regim de plata cu ora care au contract pana in 2015"<<endl;
    cout<<"\t5)Exit"<<endl;
    int op;
    while(cin>>op)
    {
        switch(op)
        {
        case 1:
            {
                for(i = 1; i <= nrp; i++)
                {
                    v[i]->afisare();
                    cout<<endl;
                }
                break;
            }
        case 2:
            {
                for(i = 1; i <= nrp; i++)
                {
                    if(dynamic_cast <Lucrativ*> (v[i]))
                    {
                        v[i]->afisare_conditionata();
                        cout<<endl;
                    }
                }
                break;
            }
        case 3:
            {
                for(i = 1; i <= nrp; i++)
                {
                    if(dynamic_cast <TESA*> (v[i]))
                    {
                        v[i]->afisare_conditionata();
                        cout<<endl;
                    }
                }
                break;
            }
        case 4:
            {
                for(i = 1; i <= nrp; i++)
                {
                    if(dynamic_cast <Contract_ora*> (v[i]))
                    {
                        v[i]->afisare_conditionata();
                        cout<<endl;
                    }
                }
                break;
            }
        case 5:
            {
                cout<<endl<<"La revedere!";
                return 0;
                break;
            }
        }
        cout<<endl<<"Urmatoarea optiune: ";
    }
    return 0;
}
