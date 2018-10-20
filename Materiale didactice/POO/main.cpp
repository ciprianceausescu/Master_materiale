#include <iostream>
#include <string.h>

using namespace std;

class referinta
{
protected:
    string tip;
public:
    referinta (string s)
    {
        tip=s;
    }
    virtual void afisare()
    {
        cout << (*this);
    }
    friend istream &operator>>(istream &i, referinta r)
    {
        cout<<"TIPUL:";
       i>>r.tip;
       return i;
    }
    friend ostream &operator<<(ostream &o, referinta r)
    {
        cout<<"TIPUL ESTE:";
        o<<r.tip;
        return o;
    }
    referinta operator = (referinta);
};
    referinta referinta::operator=(referinta r)
    {
        tip=r.tip;
        return (*this);
    }


    class tiparite: public referinta
    {
    protected:
        int nr_autori;
        string *nume, *prenume;
    public:
        void afisare()
        {
        cout << (*this);
        }
        tiparite(int n, string s):referinta(s)
        {
            nr_autori=n;
            nume=new string[nr_autori];
            prenume=new string[nr_autori];
        }
        void aloca(int n)
        {
            nume=new string[n];
            prenume=new string[n];
        }
        friend istream &operator>>(istream &i,tiparite &t)
        {
            referinta &r=t;
            i>>r;
            delete [] t.nume;
            delete [] t.prenume;
            cout<<"NUMARUL DE AUTORI:";
            i>>t.nr_autori;
            t.aloca(t.nr_autori);
            for (int j=0;j<t.nr_autori;j++)
            {
                cout<<"NUMELE AUTORULUI ESTE:";
                i>>t.nume[j];
                i>>t.prenume[j];
            }
            return i;
        }
        friend ostream &operator<<(ostream &o,tiparite &t)
        {
            referinta &r=t;
            o<<r;
            for (int j=0;j<t.nr_autori;j++)
            {
                cout<<"NUMELE AUTORULUI ESTE:";
                o<<t.nume[j];
                o<<t.prenume[j];
            }
            return o;
        }
    };


    class web:public referinta
    {
    protected:
        string nume, titlu, url;
        int zi, luna, an;
    public:
        void afisare()
        {
            cout << (*this);
        }
            web(string t, string n, string tit, string u,int z, int l, int a):referinta(t)
            {
                nume=n;
                titlu=tit;
                url=u;
                zi=z;
                luna=l;
                an=a;
            }
            friend istream &operator>>(istream &i, web &w)
            {
                referinta &r=w;
                i>>r;
                cout<<"NUMELE PROPRIETARULUI:";
                i>>w.nume;
                cout<<"TITLUL ARTICOLULUI:";
                i>>w.titlu;
                cout<<"URL-UL ARTICOLULUI:";
                i>>w.url;
                cout<<"DATA:";
                i>>w.zi;
                i>>w.luna;
                i>>w.an;
                return i;
            }
            friend ostream &operator<<(ostream &o, web &w)
            {
                referinta &r=w;
                o<<r;
                cout<<"NUMELE PROPRIETARULUI:";
                o<<w.nume;
                cout<<"TITLUL ARTICOLULUI:";
                o<<w.titlu;
                cout<<"URL-UL ARTICOLULUI:";
                o<<w.url;
                cout<<"DATA:";
                o<<w.zi;
                o<<w.luna;
                o<<w.an;
                return o;
            }

    };



    class articol:public tiparite
    {
    protected:
        string titlu, numerevista;
        int an, nrrevista, nrpagini;
    public:
       void afisare()
    {
        cout << (*this);
    }
        articol(int n, string s, string t, string nume, int a, int nrr, int nrp):tiparite(n,s)
        {
            titlu=t;
            numerevista=nume;
            an=a;
            nrrevista=nrr;
            nrpagini=nrp;
        }
        friend istream &operator>>(istream &i,articol &a)
        {
            tiparite &t=a;
            i>>t;
            cout<<"TITLUL ARTICOLULUI:";
            i>>a.titlu;
            cout<<"NUMELE REVISTEI:";
            i>>a.numerevista;
            cout<<"ANUL APARITIEI:";
            i>>a.an;
            cout<<"NUMARUL REVISTEI:";
            i>>a.nrrevista;
            cout<<"NUMARUL DE PAGINI:";
            i>>a.nrpagini;
            return i;
        }
        friend ostream &operator<<(ostream &o,articol &a)
        {
            tiparite &t=a;
            o<<t;
            cout<<"TITLUL ARTICOLULUI:";
            o<<a.titlu;
            cout<<"NUMELE REVISTEI:";
            o<<a.numerevista;
            cout<<"ANUL APARITIEI:";
            o<<a.an;
            cout<<"NUMARUL REVISTEI:";
            o<<a.nrrevista;
            cout<<"NUMARUL DE PAGINI:";
            o<<a.nrpagini;
            return o;
        }
    };

    class carte:public tiparite
    {
    protected:
        string titlu, editura, oras;
        int an;
    public:
         void afisare()
    {
        cout << (*this);
    }
        carte(int n, string s, string t, string e, string o, int a):tiparite(n,s)
        {
            titlu=t;
            editura=e;
            oras=o;
            an=a;
        }
        friend istream &operator>>(istream &i, carte &c)
        {
            tiparite &t=c;
            i>>t;
            cout<<"TITLU:";
            i>>c.titlu;
            cout<<"EDITURA:";
            i>>c.editura;
            cout<<"ORAS:";
            i>>c.oras;
            cout<<"AN:";
            i>>c.an;
            return i;
        }
        friend ostream &operator<<(ostream &o, carte &c)
        {
            tiparite &t=c;
            o<<t;
            cout<<"TITLU:";
            o<<c.titlu;
            cout<<"EDITURA:";
            o<<c.editura;
            cout<<"ORAS:";
            o<<c.oras;
            cout<<"AN:";
            o<<c.an;
            return o;
        }
    };


    int main()
    {
        cout<<"*****************************BIBLIOGRAFIE***********************************"<<endl;
        int n, s;
        cout<<"n=";
        cin>>n;
        referinta *r[300];
        web *w;
        articol *a;
        carte *c;
        //r[0]=new web("WEB", "blabla", "tralala", "blablalbal",12,12,2012);
        //r[1]=new articol (2, "tiparite", "tara", "badgagda", 1995, 12, 95);
        //r[2]=new carte (3, "tiparite", "asfaga", "msdas", "sibiu", 1977);

        for (int j=0;j<n;j++)
        {
            cout<<"Ce tip?: "<<endl;
            cout<<"1.WEB"<<endl;
            cout<<"2.ARTICOL"<<endl;
            cout<<"3.CARTE"<<endl;
            cin>>s;
            if (s==1)
            {
               r[j] = new web("WEB", "blabla", "tralala", "blablalbal",12,12,2012);
                if(w=dynamic_cast<web*>(r[j]))
            cin >> *w;
            }
            if (s==2)
            {
                r[j] = new articol(2, "tiparite", "tara", "badgagda", 1995, 12, 95);
                if(a=dynamic_cast<articol*>(r[j]))
            cin >> *a;
            }
            if (s==3)
            {

               r[j] = new carte(3, "tiparite", "asfaga", "msdas", "sibiu", 1977);
                if(c=dynamic_cast<carte*>(r[j]))
            cin >> *c;
            }
        }
cout<<"\nLISTA REFERINTA:\n\n";
    for (int j = 0; j < n; j++)
    {
        r[j]->afisare();
        cout<<endl;
    }
    return 0;
        //cin>>(web&)(*r[0]);
   // cin>>(articol&)(*r[1]);
    //cin>>(carte&)(*r[2]);*/
//    cout<<(web&)(*r[0])<<endl;
//    cout<<(articol&)(*r[1])<<endl;
//    cout<<(carte&)(*r[2]);
    }
