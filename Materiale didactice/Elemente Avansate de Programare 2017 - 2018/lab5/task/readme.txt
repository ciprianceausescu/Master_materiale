Exercitiul 1 din laboratorul 5

Implementaţi interfaţa Task (dată mai jos) în cele 3 moduri.
    public interface Task {
        // Execută acţiunea specifică taskului
        void execute();
    }

● Un task care afişează un mesaj la output. Mesajul este dat în constructor.
● Un task care reţine data la care a fost creat taskul şi se afişează un mesaj cu
această oră. Timpul se consideră cel din momentul în care este apelat un
constructor.
● Un task care contorizează numărul de instanţe generate pentru acel task. Contorul
va fi afişat după fiecare incrementare.