package lab6_lab7.sort;

class OtherSort implements ISort {
    private int a[], n;

    OtherSort(int a[], int n) {
        if (n < DIM_MAX) {
            this.a = a;
            this.n = n;
        } else {
            this.a = null;
            this.n = 0;
        }
    }

    public void sort() {
        for (int i = 0; i < n - 1; i++)
            for (int j = i + 1; j < n; j++)
                if (a[i] > a[j]) {
                    int v = a[i];
                    a[i] = a[j];
                    a[j] = v;
                }
    }

    public void afis() {
        for (int i = 0; i < n; i++)
            System.out.printf("%d ", a[i]);
        System.out.println();
    }
}