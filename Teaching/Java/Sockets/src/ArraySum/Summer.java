package ArraySum;

// CSE 373, Winter 2013, Marty Stepp
// A Summer is a task that can be run in a thread that computes the sum
// of a given range of numbers in a large array.

public class Summer implements Runnable {
    private int[] a;
    private int min, max;
    private int sum;

    public Summer(int[] a, int min, int max) {
        this.a = a;
        this.min = min;
        this.max = max;
    }

    public int getSum() {
        return sum;
    }

    public void run() {
        this.sum = ArraySum.sumRange(a, min, max);
    }
}