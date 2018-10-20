#include <cstdlib>
#include <iostream>
using namespace std;
/* sintaxa:
class clasa1  // analog struct declara un tip de date
{ tip1 data1;
   tip2 data2;
    tip3 metoda3(){}
    tip4 metodat4(){}
} ;  // s-a declarat tipul clasa1 (se numeste clasa ) cu cimpurile (se numesc date) si functiile atasate (se numesc metode )
     // orice clasa are atasate constructor de initializare, constructor de copiere, operator de = , destructor
int main()
{
clasa1 ob1,ob2; // ob1,ob2 variabile de tipul clasa (se numesc obiecte) 
//la alocare de zona pentru un obiect se apeleaza o functie speciala constructorul de initializare   adica :  clasa1()
clasa1 ob3=ob1;
// la alocare de zona si initializare (in acelasi timp ) se apeleaza constructorul de copiere (nu operatorul =)
ob1.metoda3(); // obiectul ob1 apeleaza metoda3()     
// la eliberarea zonei  alocate obiectului se apeleaza destructorul  ~clasa1()
ob1=ob2 // se apeleaza operatorul =  
}
*/

/* exemplul 1 - constructorul de initializare implicit, constructorul de copiere implicit, operatorul = implicit , destructorul implicit (date de compilator) 
    functioneaza corect

class sirb{   
      public: char v[10]; // nu e recomandat ca datele sa fie publice -pot fi modificate de oriunde
      };

int main(int argc, char *argv[])
{
    sirb s1;   //aloca 10 caractere
    strcpy(s1.v,"ab");
    sirb s2=s1; //aloca zona si initializeaza deci apeleaza constructorul de copiere care  face copiere bit cu bit din  dar au zone diferite de memorie 
     s2.v[0]='c'; // se modifica doar sirul de caractere  pentru sirul s2
    cout<<s1.v<<s2.v<<endl;
    s1=s2; // operatorul de atribuire face copiere bit cu bit dar sunt zone diferite de memorie 
    cout<<s1.v<<s2.v<<endl;
   {sirb s3;} // este apelat constructorul de initializare  si apoi destructorul  pentru ca domeniul de definitie este in instructiunea compusa  
         system("PAUSE");
    return 0;
}
*/
// exemplul 2 necesita suprascrierea constructorilor impliciti, ai operatorului de atribuire si al destructorului  pentru ca  ar adresa acelasi sir de caractere

class sir {
      char *s;
public:      sir(char *v=""){s= new char[strlen(v)+1]; strcpy(s,v); cout<<"constr param implicit"<<endl;}
// constructorul se apeleaza cind se aloca spatiu pentru  un obiect -nu si cu malloc
// daca se creaza un constructor de initializare cel implicit al compilatorului nu mai este apelat
             sir (const sir &o){s=new char[strlen(o.s)+1]; //!!! am acces la cimpurile private s(ale obiectului curent) o.s -sunt in metoda clasei
                         strcpy(s, o.s);
                         cout<<"constructor de copiere"<<endl;
                         }
   //!!! daca se scrie constructorul  de copiere -nu mai e disponibil constructorul de copiere implicit dar NICI constructorul  de initializare
   // constructorul de copiere se apeleaza cind se aloca zona pentru un obiect si se initializeaza obiectul cu un obiect existent
   //se apeleaza : 1.la transmitere parametrii prin valoare(nu referinta) intr-o functie  
     //	       2. la intoarcere prin valoare(nu referinta) a unui obiect  dintr-o functie (prin intermediul unui obiect temporar const),
   //	       3. la initializarea unui obiect cu un obiect existent
   //  parametru obligatoriu transmis prin referinta -altfel este o recursie infinita sir s4=s2 de fapt s4.sir(s2)

              sir & operator =(sir & o)
            // operator este o functie cu nume si forma speciala a=b se poate "gandi ca"  a.=(b) 
	// intoarce un obiect sir pentru a putea face mai multe operatii =
              //ex a=b=c de fapt a.=(b.=(c)) deci = intoarce obiectul ce apeleaza operatorul
              //!!! = () [] -> si operatorul de conversie la tip se pot suprascrie doar ca metode (functii membru)
                { delete []s;
                s=new char [strlen(o.s)+1];
                strcpy(s, o.s);
                return (*this); // this -in orice metoda nestatica este adresa obiectului care apeleaza metoda
                    }
               sir operator ()(int i){return *this;}
              friend istream & operator >>(istream & i, sir & sl); // cin >> o de fapt >>(cin, o)
	//functie exterioara clasei   primul parametru   nu apartine clasei sir - este de tip istream       
	//suprascrise ca friend pentru a avea acces la datele private si protected-
           // intoarce un flux de intrare pentru a putea scrie operatorul iterat : cin>>s1>>s2;
           // parametrii transmisi prin referinta pentru ca modificarile sa ramana si in programul apelant
              friend ostream & operator <<(ostream & o, sir sl );
             sir & operator +(sir &o)  // a+b se poate "gandi ca " : a.+(b)
             { sir *p=new sir ;
                p->s=new char [strlen(s)+strlen(o.s)+1];
                strcpy(p->s, s);
                strcat(p->s,o.s);
                 return (*p);
                 }

             };
  istream & operator >>(istream & i, sir & sl)
  {char c[255];
   i>>c;
   delete sl.s;
   sl.s=new char[strlen(c)+1];
   strcpy(sl.s,c);
   return i;
   }
  ostream & operator <<(ostream & o, sir sl )
  { o<<sl.s;
    return o;
  }
      sir f1(sir p){return p;}
      sir & f2(sir p, sir &r)
      {sir l;
      //return p;  return l ; nu este corect -intorc alt nume catre o zona care va fi eliberata la terminarea functiei
       sir *ps= new sir;
        //return r ; -zona ramane alocata  si dupa terminarea functiei
        return *ps; // zona ramane alocata pana se elibereaza cu delete
      }
    void f3(const sir &r){}

int main(int argc, char *argv[])
{
    sir s1, s2="abc", s3("ab"), v[2], v1[2]={"ab", sir("bc")}, *po; // constructorul se apeleaza de 1+1+1+2+2+0 ori 
    po=new sir;  // se apeleaza constructorul 
    po=new sir("abc");
    po= new sir [2]; // nu se poate face initializarea vectorului  la alocare !!!!
    
    sir s4=s2; // apeleaza constructorul de copiere 
    f1(s1); // apeleaza constructorul de copiere pentru parametrul transmis prin valoare  si pentru obiectul intors prin valoare
    f1(s1)=s2;// atribuirea se face in obiectul temporar-dar nu poate fi accesat ulterior
    f2(s1,s2);
    s4=s2;
    s1=s2+s3;
    s1(2)(3); // operatorul () poate fi  aplicat de mai multe ori
   // f2(s1,f1(s2));  nu este corect pentru ca se lucreaza cu referinta (un alt nume)  catre o zona temporara care este const -nu poate fi modificata 
     f3(f1(s2)); // functioneaza 
 
             system("PAUSE");
    return 0;
}


