package tablouri;

/**
 * Copierea unui tablou in alt tablou
 */
public class CopyArray {

    public static void main(String[] args) {

        int a[] = {1, 2, 3, 4};
        int b[] = new int[4]; // Atentie, b trebuie alocat

        // Varianta 1
        for (int i = 0; i < a.length; i++) {
            b[i] = a[i];
        }

        // Varianta 2
        System.arraycopy(a, 0, b, 0, a.length);

        /**
         * Varianta 3 - Nu are efectul dorit!!!
         * O atribuire de genul b = a are altă semnificaţie decât copierea
         * elementelor lui a în b şi nu poate fi folosită în acest scop.
         * Este o a tribuire de referinţe, în urma acestei atribuiri variabilele
         * b şi a vor referi acelaşi obiect (vector). Dacă modificăm un element
         * al lui a se modifică şi b şi invers.
         */
        b = a;
        System.out.println(a[0] + " " + b[0]);
        b[0] = 5;
        System.out.println(a[0] + " " + b[0]);
        a[0] = 6;
        System.out.println(a[0] + " " + b[0]);
    }
}
