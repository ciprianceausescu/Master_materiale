package lab6_lab7.sort;

import java.util.Arrays;


public class SortJava implements ISort {

    private float f[];
    private int n;

    SortJava(float f[], int n) {
        if (n < DIM_MAX) {
            this.f = f;
            this.n = n;
        } else {
            this.f = null;
            this.n = 0;
        }
        // DIM_MAX=n;-nu se poate, este constant
    }

    public void sort() {
        Arrays.sort(f);
    }

    public void afis() {
        for (int i = 0; i < n; i++)
            System.out.printf("%.2f ", f[i]);
        System.out.println();
    }

    float sum() {
        float s = 0;
        if (n > 0)
            for (float x : f) s += x;
        return s;
    }
}
