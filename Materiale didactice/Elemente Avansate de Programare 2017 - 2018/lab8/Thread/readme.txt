Sa ne amintim:

- Facilitatea de lucru cu thread-uri in Java este oferita de clasa Thread sau de interfata Runnable.
- In ambele cazuri, trebuie implementata metoda run, unde se specifica efectiv codul ce va rula
  pe un thread.

- De exemplu:

       @Override
       public class MyClass exteds Thread {

            public void run() {
                // do something
            }
       }

       SAU

       @Override
       public class MyClass implements Runnable {

            public void run() {
                // do something
            }
       }

- Pentru a porni un thread, apelam metoda "start", respectiv:

        new MyClass().start(), daca MyClass extends Thread
        new Thread(new MyClass).start(), daca MyClass implements Runnable.
