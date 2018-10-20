package lab4_lab5;

/**
 * Exemplu de agregare de clase. Clasa Book
 * are ca membru o lista de pagini, encapsulate
 * in clasa Page. Practic, va refolosi codul din
 * clasa Page.
 */
class Book {
    private Page[] pages;

    public Book(int dim, String title) {
        pages = new Page[dim];
        for (int i = 0; i < dim; i++)
            pages[i] = new Page("Pagina " + i, i);
    }
}
