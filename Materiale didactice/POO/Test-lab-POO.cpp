#include <iostream>
#include <cstdlib>
#include <stdlib.h>
#include <cstring>
#include <string>
#include <Windows.h>

using namespace std;

class produs
{
private:
    string denumire;
    string unitate_masura;
    int valabilitate;
    int discount;

public:
    produs (string nume="Necunoscut", string mas="Bucata", int val=0, int disc=0)
    {
        denumire=nume;
        unitate_masura=mas;
        valabilitate=val;
        discount=disc;
    }

    produs &operator=(produs &o)
    {
        denumire=o.denumire;
        unitate_masura=o.unitate_masura;
        valabilitate=o.valabilitate;
        discount=o.discount;
        return (*this);
    }

    int get_valabilitate()
    {
        return valabilitate;
    }

    string get_denumire()
    {
        return denumire;
    }

    int get_discount()
    {
        return discount;
    }

    friend istream& operator>> (istream& input, produs &o);
    friend ostream& operator<< (ostream& output, produs o);
    friend void afisare_produs(produs o);
    friend void afis_masura(produs o);
};


    void afisare_produs(produs o)
    {

        cout<<o.unitate_masura<<" de "<<o.denumire;
        if(o.valabilitate>0)
           cout<<", cu termen de valabilitate de "<<o.valabilitate<<" zile ";
    }

    void afis_masura(produs o)
    {
        cout<<o.unitate_masura;
        if(o.discount>0)
            cout<<", la care decide sa aplice un discount de "<<o.discount<<"%";
    }

istream& operator>>(istream& input, produs &o)
{
    int k=0;
    cout<<"Denumire produs: ";
    input>>o.denumire;
    cout<<"Unitatea de masura: \n";
        cout<<"  1.Bucata\n  2.Kilogram\n  3.Litru\n  ";
    cin>>k;
    while(k>3 || k<1)
    {
        cout<<"Introduceti o valoare valida!\n  ";
        cin>>k;
    }
    if(k==1)

        o.unitate_masura="Bucata";
    else if(k==2)
        o.unitate_masura="Kilogram";
    else if(k==3)
        o.unitate_masura="Litru";

    cout<<"Valabilitate: ";
    input>>o.valabilitate;
    cout<<"Discount: ";
    input>> o.discount;

    return input;
}


ostream& operator<<(ostream& output, produs o)
{
    output<<o.denumire<<" la "<<o.unitate_masura;
    if(o.valabilitate>0)
    {
        output<<", termen de valabilitate de "<<o.valabilitate<<" zile";
    }

    if(o.discount>0)
    {
        output<<", cu discount de "<<o.discount<<"%";
    }
    output<<endl;

    return output;

}


class lot
{
private:
    produs p;
    int cantitate;
    string data;
    double pret;
    double pret_final;
    int zi_exp;
    int luna_exp;
    int an_exp;

public:

    lot()
    {
        produs r;
        p=r;
        cantitate=0;
        data="01.01.2016";
        pret=0;
    }

    lot(produs prod, int c=0, string d="01.01.2016",double pr=0)
    {
        int zi=1,luna=1,an=0,v=0, maxim=0;
        p= prod;
        cantitate=c;
        data=d;
        pret=pr;
        an=atoi(data.substr(6,4).c_str());
        luna=atoi(data.substr(3,2).c_str());
        zi= atoi(data.substr(0,2).c_str());
        v=p.get_valabilitate();
        zi=zi+v;
        if(luna==1||luna==3||luna==5||luna==7||luna==8||luna==10||luna==12)
            maxim=31;
        if(luna==4||luna==6||luna==9||luna==11)
            maxim=30;
        if(luna==2)
        {
           if(an%4==0)
            maxim=29;
           else
            maxim=28;
        }

        while(zi>maxim)
        {
            zi=zi-maxim;
            luna++;
            if(luna==13)
            {
                an++;
                luna=1;
            }
            if(luna==1||luna==3||luna==5||luna==7||luna==8||luna==10||luna==12)
                maxim=31;
            if(luna==4||luna==6||luna==9||luna==11)
                maxim=30;
            if(luna==2)
                maxim=28;

        }

          zi_exp=zi;
          luna_exp=luna;
          an_exp=an;

        if(v==0)
        {
            zi_exp=0;
            luna_exp=0;
            an_exp=0;
        }

        if(p.get_discount()>0)
        {
            int pp;
            pp=p.get_discount();
            pret_final=pret-((pret * (double)pp)/100 );
        }
        else pret_final=pret;


    }

    void convert(int &an, int &luna, int &zi)
    {
        an=atoi(data.substr(6,4).c_str());
        luna=atoi(data.substr(3,2).c_str());
        zi= atoi(data.substr(0,2).c_str());

    }

    int verificare_valabilitatae(int zi, int luna, int an)
    {
            int data1 = 0, data2 = 0;

            if(an_exp==0&&luna_exp==0&&zi_exp==0)
            {
                return 1;
            }

            data1 = an_exp *10000 + luna_exp*100 + zi_exp;
            data2 = an *10000 + luna*100 + zi;
            //cout<<data1<<endl<<data2<<endl<<endl;
            if(data1 >= data2)
                return 1;
            else
                return 0;
    }

    int verif_vanzare(string prod, int ct)
    {
        if( prod==p.get_denumire() && cantitate>=ct )
        {
            SYSTEMTIME st;
            GetLocalTime(&st);

            int zi_c=0, luna_c=0, an_c=0;
            zi_c = st.wDay;
            luna_c = st.wMonth;
            an_c = st.wYear;


            if(this->verificare_valabilitatae(zi_c,luna_c,an_c)==1)
                return 1;
            else
                return 0;
        }
        else if(prod==p.get_denumire()&& cantitate<ct)
        {
            SYSTEMTIME st;
            GetLocalTime(&st);

            int zi_c=0, luna_c=0, an_c=0;
            zi_c = st.wDay;
            luna_c = st.wMonth;
            an_c = st.wYear;

            if(this->verificare_valabilitatae(zi_c,luna_c,an_c)==1)
                return 2;
            else
                return 0;

        }
        else return 0;
    }


    double get_pret()
    {
         return pret_final;
    }

    void vanzare(int c)
    {
        double tot=0;
        cout<<"Pret TOTAL: ";
        double t;
        t=get_pret();
        tot=(double)c * t ;
        cout<<tot<<" lei\n";
        cantitate=cantitate-c;
        cout<<"\nVanzare efectuata cu succes!\n";

    }

    friend istream& operator>> (istream& input, lot &o);
    friend ostream& operator<< (ostream& output, lot o);


};

 istream& operator>> (istream& input, lot &o)
{
    cout<<"Produsul din lot: \n";
    input>>o.p;
    cout<<"Cantitate: ";
    input>>o.cantitate;
    cout<<"Data (ZZ.LL.AAAA): ";
    input>>o.data;
    cout<<"Pret: ";
    input>>o.pret;

        int zi=1,luna=1,an=0,v=0, maxim=0;
        an=atoi(o.data.substr(6,4).c_str());
        luna=atoi(o.data.substr(3,2).c_str());
        zi= atoi(o.data.substr(0,2).c_str());
        v=o.p.get_valabilitate();
        zi=zi+v;
        if(luna==1||luna==3||luna==5||luna==7||luna==8||luna==10||luna==12)
            maxim=31;
        if(luna==4||luna==6||luna==9||luna==11)
            maxim=30;
        if(luna==2)
        {
           if(an%4==0)
            maxim=29;
           else
            maxim=28;
        }

        while(zi>maxim)
        {
            zi=zi-maxim;
            luna++;
            if(luna==13)
            {
                an++;
                luna=1;
            }
            if(luna==1||luna==3||luna==5||luna==7||luna==8||luna==10||luna==12)
                maxim=31;
            if(luna==4||luna==6||luna==9||luna==11)
                maxim=30;
            if(luna==2)
                maxim=28;

        }

          o.zi_exp=zi;
          o.luna_exp=luna;
          o.an_exp=an;


          if(v==0)
        {
            o.zi_exp=0;
            o.luna_exp=0;
            o.an_exp=0;
        }


        if(o.p.get_discount()>0)
        {
            int pp;
            pp=o.p.get_discount();
            o.pret_final=o.pret-((o.pret * (double)pp)/100 );
        }
        else o.pret_final=o.pret;

    return input;
}

ostream& operator<< (ostream& output, lot o)
{
    output<<endl;
    output<<"Pe data de "<<o.data<<", magazinul primeste un lot de "<<o.cantitate<<" de ";
    afisare_produs(o.p);
    output<<", la pretul de "<<o.pret<<" lei pe ";
    afis_masura(o.p);

    if(o.zi_exp!=0&& o.luna_exp!=0&&o.an_exp!=0)
    output<<", exipira la data de: "<<o.zi_exp<<"."<<o.luna_exp<<"."<<o.an_exp;

    output<<endl;

    return output;
}



int meniu()
{
    int x;

    cout<<endl;
    cout<<"       **** MAGAZIN ****   \n\n";
    cout<<" 1.Adauga un produs nou\n";
    cout<<" 2.Afiseaza toate produsele\n";
    cout<<" 3.Adauga un lot nou\n";
    cout<<" 4.Afiseaza toate loturile\n";
    cout<<" 5.Afisarea loturilor dintr-o anumita perioada data\n";
    cout<<" 6.Afisarea loturilor disponibile\n";
    cout<<" 7.Vanzarea unui produs\n";
    cout<<" 8.Exit\n ";
    cin>>x;
    while(x>8||x<1)
    {
        cout<<"INTRODUCETI O VALOARE VALIDA!!!\n";
        cin>>x;
    }

    return x;

}




int main()
{

     produs v[100]=
     {
         produs ("Paine","Bucata",5,10),
         produs ("Paine","Bucata",8,0),
         produs ("Cirese","Kilogram",5,10),
         produs ("Tricou","Bucata",0,25),
         produs ("Lapte","Litru",7,2)
     };


     lot l[100]=
     {
         lot (v[0],200,"25.05.2016",3),
         lot (v[1],50,"13.05.2016",2),
         lot (v[2],10,"15.05.2016",8),
         lot (v[3],30,"11.05.2016",20)

     };


    int i=5,j=4;
    int m;

    do {
    m=meniu();
    if(m==1)
    {
        system("CLS");
        cin>>v[i];
        cout<<endl<<" Produsul a fost adaugat!\n";
        i++;
    }

    else if(m==2)
    {
        system("CLS");
        for(int k=0;k<i;k++)
            cout<<v[k];
    }

    else if(m==3)
    {
        system("CLS");
        cin>>l[j];
        cout<<endl<<" Lotul a fost adaugat!\n";
        j++;
    }
    else if (m==4)
    {
        system("CLS");
        for(int k=0;k<j;k++)
            cout<<l[k];
    }

    else if (m==5)
    {
     system("CLS");
     string p1,p2;
     cout<<"Intre data (ZZ.LL.AAAA): ";
     cin>>p1;
     cout<<"Si data (ZZ.LL.AAAA): ";
     cin>>p2;
     int an1=atoi(p1.substr(6,4).c_str());
     int luna1=atoi(p1.substr(3,2).c_str());
     int zi1= atoi(p1.substr(0,2).c_str());
     int an2=atoi(p2.substr(6,4).c_str());
     int luna2=atoi(p2.substr(3,2).c_str());
     int zi2= atoi(p2.substr(0,2).c_str());
     int a=0,lu=0,z=0,t=0;
     for (int k=0;k<j;k++)
     {
            l[k].convert(a,lu,z);

            int data1 = 0, data2 = 0, data3 = 0;

            data1 = an1 *10000 + luna1*100 + zi1;
            data2 = an2 *10000 + luna2*100 + zi2;
            data3 = a * 10000 + lu * 100 + z;

            if(data3 >= data1 && data3 <= data2)
                    cout<<l[k];
            else t++;

     }
        if (t==j)
        cout<<" NU exista loturi in aceasta perioada\n";


    }

    else if(m==6)
    {
        system("CLS");
        SYSTEMTIME st;
        GetLocalTime(&st);

        int zi_c=0, luna_c=0, an_c=0,q=0;
        zi_c = st.wDay;
        luna_c = st.wMonth;
        an_c = st.wYear;

        for (int k=0;k<j;k++)
        {
            if (l[k].verificare_valabilitatae(zi_c,luna_c,an_c)==1)
                cout<<l[k];
            else
                q++;

            if(q==j)
                cout<<" NU exista loturi disponibile\n";
        }

    }

    else if (m==7)
    {
        system("CLS");
        string name;
        int cant=0,u=0,a[100],b=0,h[100],z=0;

        for(int i=0;i<100;i++)
        {a[i]=-1;}
         b=0;
        for(int i=0;i<100;i++)
        {h[i]=-1;}
         z=0;

        cout<<"Dati numele produsului: ";
        cin>>name;
        cout<<"Dati cantitate: ";
        cin>>cant;

        for (int k=0;k<j;k++)
        {
            if(l[k].verif_vanzare(name,cant)==1)
                {
                    a[b]=k;
                    b++;
                }
            else if( l[k].verif_vanzare(name,cant)==2)
                {
                    h[z]=k;
                    z++;
                }

            else  u++;
        }

        if(a[0]>=0)
        {
            double mini;
            int ini;
            ini=a[0];
            mini=l[a[0]].get_pret();
            for (int i=0;i<b;i++)
            {
                 if(l[a[i]].get_pret()<mini)
                {
                    mini=l[a[i]].get_pret();
                    ini=a[i];
                }
            }
            l[ini].vanzare(cant);
        }
        else if(a[0]<0&&h[0]>=0)
        {
            cout<<"\n Loturi disponibile dar in cantitati insuficiente:\n ";
            for(int i=0; i<z;i++)
            {
                cout<<l[h[i]];
            }
        }

        if(u==j)
            cout<< "Nu exista produse disponibile pentru vanzare!\n";


    }

}while (m!=8);

    return 0;
}
