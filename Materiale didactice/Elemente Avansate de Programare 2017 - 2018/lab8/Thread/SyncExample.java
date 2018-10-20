package lab11;

/**
 * Un exemplu de lucru cu un obiect sincronizat.
 */
public class SyncExample {

    int x;

    public SyncExample(int x) {
        this.x = x;
    }

    public int getX() {
        return x;
    }

    public void increment() {
        x = x + 1;
    }

    static class Runn implements Runnable {

        String name;
        SyncExample ob;

        public Runn(String name, SyncExample ob) {
            this.name = name;
            this.ob = ob;
        }

        @Override
        public void run() {
            for (int i = 1; i <= 10; ++i) {
                int x;

                // Ne asiguram ca obiectul ob nu este
                // modificat in acelasi timp de doua
                // threaduri diferite.
                synchronized (ob) {
                    ob.increment();
                    x = ob.getX();
                }
                System.out.println("From thread " + name + ": x = " + x);
            }
        }
    }

    public static void main(String[] args) {

        SyncExample ob = new SyncExample(0);

        // Pornim 10 threaduri.
        for (int i = 1; i <= 10; ++i) {
            new Thread(new Runn("thread" + i, ob)).start();
        }

    }
}
