import java.util.Scanner;

public class Armstrong {
    public static int solve( int n, int[] a, int low, int high) {
        int s = 0;
        if(n < 1 || n > 10 || a == null || low < 0 || low >= n || high < 0 || high >= n) {
            System.out.println("Conditions not met.");
            return -1;
        }
        for ( int i = 0 ; i < n ; ++i  ) {
            if(a[i] < 0) {
                System.out.println("Conditions not met.");
                return -1;
            }
        }
        for(int i = low; i <= high; ++i) {
            if(isArmstrongNumber(a[i])) {
                s += a[i];
            }
        }
        System.out.println(s);
        return s;
    }

    public static boolean isArmstrongNumber(int x) {
        int  sum = 0, temp, remainder, digits = 0;
        temp = x;
        // Count number of digits
        while (temp != 0) {
            digits++;
            temp = temp/10;
        }
        temp = x;
        while (temp != 0) {
            remainder = temp%10;
            sum = sum + power(remainder, digits);
            temp = temp/10;
        }
        if (x == sum)
            return true;
        else
            return false;
    }

    static int power(int n, int r) {
        int c, p = 1;
        for (c = 1; c <= r; c++)
            p = p*n;
        return p;
    }
    public static void main(String[] args) {
        // TODO Auto-generated method stub
        int n = 5;
        int[] a = {3, 4, 5, 8, 9};
        int low, high;
        Scanner sc = new Scanner(System.in);
        low = sc.nextInt();
        high = sc.nextInt();
        System.out.println(Armstrong.solve(n, a, low, high));
    }
}
