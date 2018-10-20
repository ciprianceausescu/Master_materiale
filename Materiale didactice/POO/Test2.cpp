#include<iostream>
#include<string>
#include<cstring>

using namespace std;

///clasa de baza din care vom mosteni referintele tiparite Carte si Revista
class ReferintaTiparita
{
 protected:
    struct Autor
    {
        char* nume;
        char* prenume;
        Autor(const char* nm,const char* pr)
        {
            nume = new char[strlen(nm)+1];
            strcpy(nume,nm);
            prenume = new char[strlen(pr)+1];
            strcpy(prenume,pr);
        }
    }aut;
    char* titlu;
    int an;
public:
    ReferintaTiparita(const char* tit, int a, Autor autt):an(a),aut(autt)
    {
        titlu = new char[strlen(tit)+1];
        strcpy(titlu,tit);
    }
    virtual  void afisare_referinta()=0;  ///Functile de afisare ce sunt pur virtuale si trebuie suprascrise / definite in clasele
    virtual void afisare_referinta(int)=0; ///pe care le vom deriva din aceasta
    virtual void afisare_referinta(const char*)=0;
};
class Carte:public ReferintaTiparita
{
     char* nume_edit;
     char* oras_edit;
public:
     Carte(const char* tit,int a,const char* nume,const char* prenume,const char* ne,const char* oe):ReferintaTiparita(tit,a,Autor(nume,prenume))
     {
         nume_edit = new char[strlen(ne)+1];
         strcpy(nume_edit,ne);
         oras_edit = new char[strlen(oe)+1];
         strcpy(oras_edit,oe);
     }
     void afisare_referinta()
     {
        cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_edit<<", "<<oras_edit<<endl;
     }
      void afisare_referinta(int a)
      {
          if(an==a)
            cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_edit<<", "<<oras_edit<<endl;
      }
      void afisare_referinta(const char* nume)
      {
          if(strcmp(aut.nume,nume)==0)
              cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_edit<<", "<<oras_edit<<endl;

      }

};
class Articole:public ReferintaTiparita
{
    int numar_rev;
    char* nume_rev;
    int pi;
    int pf;
public:
     Articole(const char* tit,int a,const char* nume,const char* prenume,int nr,const char* nrv,int i,int f):ReferintaTiparita(tit,a,Autor(nume,prenume)),numar_rev(nr),pi(i),pf(f)
     {
         nume_rev = new char[strlen(nrv)+1];
         strcpy(nume_rev,nrv);
     }
     void afisare_referinta()
     {
        cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_rev<<","<<numar_rev<<", "<<pi<<"-"<<pf<<endl;
     }
       void afisare_referinta(int a)
       {
           if(an==a)
              cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_rev<<","<<numar_rev<<", "<<pi<<"-"<<pf<<endl;

       }
       void afisare_referinta(const char* nume)
       {
           if( strcmp(nume,aut.nume) == 0 )
              cout<<aut.nume<<" "<<aut.prenume<<";"<<titlu<<", "
        <<an<<", "<<nume_rev<<","<<numar_rev<<", "<<pi<<"-"<<pf<<endl;
       }
};
class Web ///Clasa de referinte Web este o clasa de sine statatoare
{
    char* propietar;
    char* titlu;
    char* URL;
    struct data
    {
        int ziua;
        int luna;
        int an;
        data(int z,int l,int a):ziua(z),luna(l),an(a){}
    } Data;
public:
    Web(const char* pr,const char* tit,const char* url,int ziua,int luna,int an):Data(ziua,luna,an)
    {
        propietar = new char[strlen(pr)+1];
        strcpy(propietar,pr);
        titlu = new char[strlen(tit)+1];
        strcpy(titlu,tit);
        URL = new char[strlen(url)+1];
        strcpy(URL,url);
    }
    void afisare_referinta()
    {
         cout<<propietar<<" : "<<titlu<<" . "<<URL<<"(accesat "<<Data.ziua<<"."<<Data.luna<<"."<<Data.an<< " )"<<endl;
    }
    void afisare_referinta(const char* propietar)
    {
         if( strcmp(propietar,this->propietar) == 0)
           cout<<propietar<<" : "<<titlu<<" . "<<URL<<"(accesat "<<Data.ziua<<"."<<Data.luna<<"."<<Data.an<< " )"<<endl;
    }
};
int main()
{
     ReferintaTiparita* vt[100]; ///Un vector de pointeri de tipul clasei de baza , vom instantia tipuri diferite , iar legarea
     Web* vw[50];  ///la functii se va face la executia programului
     /// vectori de pointeri catre tipul Web
     int op=0,nr1=0,nr2=0; ///Numarul de elemente il vom stii la executie, utilizator va introduce date , binenteles , nu o sa poata sa introduca mai mult de 100 de referinte scrise si 50 de referinte web (capacitatea maxima a celor doi vectori)
     cout<<"Introduceti tipul de referinta :\n\t1)Carte\n\t2)Articol\n\t3)Pagina Web\n\t4)Exit\n";
     while(op!=4)
     {
         cin>>op;
         switch(op)
         {
            case 1:
            {
                 char nume[100],prenume[100],titlul[100],oras[100],nume_edit[100];
                 int an;
                 cin.get();
                 cout<<endl<<"Nume autor carte: ";
                 cin.get(nume,100);
                 cin.get();
                 cout<<endl<<"Prenume autor carte: ";
                 cin.get(prenume,100);
                 cin.get();
                 cout<<endl<<"Titlu carte: ";
                 cin.get(titlul,100);
                 cin.get();
                 cout<<endl<<"Oras editura: ";
                 cin.get(oras,100);
                 cin.get();
                 cout<<endl<<"Nume editura : ";
                 cin.get(nume_edit,100);
                 cout<<endl<<"An aparitie: ";
                 cin>>an;
                 vt[nr1]=new Carte(titlul,an,nume,prenume,nume_edit,oras);
                 ++nr1;
                 break;
            }
            case 2:
            {
                char nume_rev[100],nume[100],prenume[100],titlul[100];
                int an,numar,pi,pf;
                cin.get();
                cin.get(nume_rev,100);
                cin.get();
                cin.get(nume,100);
                cin.get();
                cin.get(prenume,100);
                cin.get();
                cin.get(titlul,100);
                cin>>an>>numar>>pi>>pf;
                vt[nr1]=new Articole(titlul,an,nume,prenume,numar,nume_rev,pi,pf);
                nr1++;
                break;
            }
            case 3:
            {
                char propietar[100],titlu[100],URL[100];
                int ziua,luna,an;
                cin.get();
                cin.get(propietar,100);
                cin.get();
                cin.get(titlu,100);
                cin.get();
                cin.get(URL,100);;
                cin>>ziua>>luna>>an;
                vw[nr2]=new Web(propietar,titlu,URL,ziua,luna,an);
                nr2++;
                break;
            }
         }
         cout<<endl<<"Introduceti urmatoarea referinta:";
     }
     op=0;
     cout<<"Selectati optiunea de vizionare a referintelor:\n1)Afiseaza toate referintele\n2)Afiseaza toate referintele tiparite aparute intr-un anumit an\n3)Afiseaza toate referintele tiparite dupa numele unui autor\n4)Afiseaza toate referintele Web dupa un anumit propietar\n5)Exit\n";
     while(op!=5)
     {
         cin>>op;
         switch(op)
         {
            case 1:
            {
                for(int i=0;i<nr1;i++)
                     vt[i]->afisare_referinta();
                for(int i=0;i<nr2;i++)
                     vw[i]->afisare_referinta();
                break;
            }
            case 2:
            {
               int an;
               cin>>an;
               for(int i=0;i<nr1;i++)
                  vt[i]->afisare_referinta(an);
               break;
            }
            case 3:
            {
                char nume[100];
                cin.get();
                cin.get(nume,100);
                for(int i=0;i<nr1;i++)
                    vt[i]->afisare_referinta(nume);
                break;
            }
            case 4:
            {
                char propietar[100];
                cin.get();
                cin.get(propietar,100);
                for(int i=0;i<nr2;i++)
                    vw[i]->afisare_referinta(propietar);
                break;
            }
         }
         cout<<"Selectati optiunea de vizionare a referintelor:\n1)Afiseaza toate referintele\n2)Afiseaza toate referintele tiparite aparute intr-un anumit an\n3)Afiseaza toate referintele tiparite dupa numele unui autor\n4)Afiseaza toate referintele Web dupa un anumit propietar\n5)Exit\n";
     }
     return 0;
}

