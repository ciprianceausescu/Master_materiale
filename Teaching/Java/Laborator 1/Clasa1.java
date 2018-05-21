import java.lang.Integer;
import java.lang.String;
import java.util.Scanner;

public class Clasa1 {

    public void prim() {
        int a = 17;
        boolean prim = true;

        if (a == 1)
            prim = false;
        if (a == 2)
            prim = false;
        else
            for (int i = 3; i < Math.sqrt(a) + 1; i += 2) {
                if (a % i == 0) {
                    prim = false;
                    break;
                }
            }
        System.out.println(prim);
    }

    public void putere() {
        int a = 64;

        if (Integer.bitCount(a) == 1) {
            System.out.println("M1:Este putere a lui doi");
        }

        int nr = 0;
        String str = Integer.toBinaryString(a);
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) == '1') {
                nr++;
            }
        }
        if (nr == 1) {
            System.out.println("M2:Este putere a lui doi");
        }
    }

    public void nr_biti() {
        int v[] = {16, 17, 25, 30};

        for (int elem : v) {
            int nr = 0;
            String str = Integer.toBinaryString(elem);
            for (int i = 0; i < str.length(); i++)
                if (str.charAt(i) == '1')
                    nr++;

            if (nr % 2 == 0)
                System.out.println(elem + " este de tip 0");

            if (nr % 2 != 0)
                System.out.println(elem + " este de tip 1");
        }
    }

    public void arie() {
        float PI = (float) 3.14;
        int raza = 3;
        int lungime = 4;
        int latime = 6;
        double arie = 0;

        Scanner input = new Scanner(System.in);

        System.out.println("Optiunea:");
        String actiune = input.nextLine();

        switch (actiune) {
            case "cerc": {
                arie = Math.PI * Math.pow(raza, 2);
                break;
            }
            case "drept":
                arie = lungime * latime;

        }

        System.out.println(arie);
    }

    public void telefon() {

        Scanner sc = new Scanner(System.in);
        System.out.println("Nr telefon: ");
        String input = sc.nextLine();
        ;

        switch (input.length()) {
            case 10:
                if (regula2(input, 0))
                    System.out.println("Valid");
                else
                    System.out.println("Invalid");
                break;
            case 11:
                if (input.charAt(0) == '0' && regula2(input, 1))
                    System.out.println("Valid");
                else
                    System.out.println("Invalid");
                break;
            case 12:
                if (input.charAt(0) == '9' && input.charAt(1) == '1' && regula2(input, 2))
                    System.out.println("Valid");
                else
                    System.out.println("Invalid");
                break;
            default:
                System.out.println("Invalid");
        }
    }

    static boolean regula2(String str, int index) {
        if (str.charAt(index) == '7' || str.charAt(index) == '8' || str.charAt(index) == '9')
            return true;

        return false;
    }

    public void palindrom() {
        Scanner sc = new Scanner(System.in);
        System.out.println("Cuvant: ");
        String input = sc.nextLine();

        boolean palindrome = true;

        for (int i = 0; i < input.length(); i++) {
            if (input.charAt(i) != input.charAt(input.length() - i - 1))
                palindrome = false;
        }

        System.out.println(palindrome);
    }

    public void interval() {

        int a = 0, b = 0;
        Scanner sc = new Scanner(System.in);
        System.out.println("Intervalul :");
        a = sc.nextInt();
        b = sc.nextInt();

        System.out.println("Numarul :");
        int number = sc.nextInt();

        if (number < a || number > b)
            System.out.println("Nu este in interval");
        else if (number % 2 != 0)
            System.out.println("Nu este par");
        else {
            for (int i = 2; i < number; i++) {
                if (checkPrime(i) && checkPrime(number - i)) {
                    System.out.println("Verifica");
                    System.out.println(i + " " + (number - i));
                }

            }
        }
    }

    private static boolean checkPrime(int number) {
        if (number == 1)
            return false;
        if (number % 2 == 0)
            return false;
        for (int i = 3; i < Math.sqrt(number) + 1; i = i + 2)
            if (number % i == 0)
                return false;

        return true;
    }

    public void pozitie(){
        int a=0, b=0;
        Scanner sc = new Scanner(System.in);
        System.out.println("Introduceti pozitiile");
        a = sc.nextInt();
        b = sc.nextInt();

        System.out.println("n=");
        int number = sc.nextInt();

        String numberString = Integer.toBinaryString(number);
        System.out.println(numberString);
        StringBuilder sb = new StringBuilder(numberString);
        char temp1 = sb.charAt(b);
        char temp2 = sb.charAt(a);
        sb.setCharAt(numberString.length()-b-1, temp2);
        sb.setCharAt(numberString.length()-a-1, temp1);

        System.out.println(sb.toString());
        System.out.println(Integer.parseInt(sb.toString(), 2));
    }
}
