#include <cstdlib>
#include <iostream>
#include<cstring>
using namespace std;
template <class T> void fs(T t){cout<<"supraincarc sablon T"<<endl;}
template <class T> void fs(T p1,T p2){cout<<"supraincarc sablon T T ";}
template <> void fs(float pf){cout<<"supraincarcare template <>"<<endl;}
void fs(float pf){cout<<"supraincarcare non-template"<<endl;}
//template <class T1,class T2> void fs(T1 p1,T2 p2){cout<<"supraincarcare T1 T2"<<endl;}

int main(int argc, char *argv[])
{ float fa=2.5;
  fs('a');// deducere T=char
  fs(2.5);//deducere T=double
  fs(fa);  //non-template pt float , prioritar fata de template<>, prioritar fata de template
  fs<>(fa); //non- template   pt float, prioritar fata de template<>, prioritar fata de template
  fs<float>(fa); // template<>  priporitar fata de SABLON T=float

// fs(2,3.5);//NU FACE CONVERSIE
    fs<int>(2,3.5);// deducere explicita T=int
    fs(2,3);


 system("PAUSE");
    return 0;
}
