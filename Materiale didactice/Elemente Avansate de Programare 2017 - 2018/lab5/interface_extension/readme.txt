Exemplul din laboratorul 5, pagina 5.

- IB extinde IA
- ID extinde IB si IC.

Constanta c va fi definita intr-una sau mai multe interfete.
Putem deosebi două cazuri:
1) Constanta c este redeclarată în interfaţa ID: o referire la c constituie o referire la
constanta c din ID. Putem face însă referire şi la constantele din celelalte interfeţe prin IA.c,
IB.c şi IC.c.
2) Dacă c este declarată în mai multe interfețe, atunci când este folosită constanta c
într-o clasă ce implementează interfața ID, aceasta se va referi la cea mai recentă declarare a ei.
Dacă c este declarată în IB și IC și nu este redeclarată în ID, atunci la folosire, trebuie
specificată explicit din ce interfață să se citească c. Acest lucru se întâmplă deoarece IB și IC se
află pe același nivel în ierarhie.


În cazul metodelor, dacă în interfețele IB și IC (sau în supertipuri ale lor) apare o
metodă cu același nume, atunci avem situațiile:
1) dacă metodele au signaturi diferite, vor fi moștenite ambele metode;
2) dacă metodele au aceeaşi signatură şi acelaşi tip pentru valoarea întoarsă, va fi
moştenită o singură metodă;
3) dacă metodele au aceeaşi signatură, dar tipurile valorilor întoarse diferă, atunci
moştenirea nu va fi posibilă (eroare de compilare).