package Proiect1;
import org.hamcrest.MatcherAssert;
import org.junit.Before;
import org.junit.Test;

import java.util.stream.IntStream;

import static Proiect1.verificareVector.verificare;
import static junit.framework.TestCase.assertFalse;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertTrue;

/**
 * Created by Ciprian Mihai on 3/14/2017.
 */
public class teste {
    public int[] gol = new int[]{};
    @Test
    public void equivalencePartitioning() {
        int v[] = new int[]{1, 2, 3, 4, 5};
        assertTrue(verificare(5, v, 4));
        assertFalse(verificare(5, v, 3));
        assertFalse(verificare(5, v, -1));
        assertFalse(verificare(5, v, 5));
        int p[] = new int[]{1, -1, 2, 3, 4};
        assertFalse(verificare(5, p, 1));
        assertFalse(verificare(0, gol, 0));
        assertFalse(verificare(51, gol, 0));
    }
    @Test
    public void boundaryValueAnalysis() {
        assertFalse(verificare(0, gol, 0));
        assertFalse(verificare(0, gol, 49));
        assertTrue(verificare(1, new int[]{1}, 0));
        assertFalse(verificare(1, new int[]{1}, 49));
        int v[] = IntStream.rangeClosed(0, 49).map(x -> x = 3).toArray();
        assertTrue(verificare(50, v, 0));
        assertFalse(verificare(50, v, 49));
        int p[] = IntStream.rangeClosed(0, 50).map(x -> x = 3).toArray();
        assertFalse(verificare(51, p, 0));
        assertFalse(verificare(51, p, 49));
    }
    @Test
    public void categoryPartitioning() {
        assertFalse(verificare(-1, new int[]{}, 0));
        assertFalse(verificare(0, new int[]{}, 0));
        assertFalse(verificare(1, new int[]{2}, -1));
        assertTrue(verificare(1, new int[]{2}, 0));
        assertFalse(verificare(1, new int[]{2}, 2));
        assertFalse(verificare(1, new int[]{-1}, 1));
        assertFalse(verificare(2, new int[]{1, 2}, -1));
        assertTrue(verificare(2, new int[]{2, 2}, 0));
        assertFalse(verificare(2, new int[]{2, 2}, 3));
        assertFalse(verificare(2, new int[]{-1, 1}, 1));
        int v[] = IntStream.rangeClosed(0, 49).map(x -> x = 2).toArray();
        assertFalse(verificare(50, v, -1));
        assertTrue(verificare(50, v, 0));
        assertFalse(verificare(50, v, 51));
        int p[] = IntStream.rangeClosed(0, 49).map(x -> x = -2).toArray();
        assertFalse(verificare(50, p, 0));
        assertFalse(verificare(51, new int[]{}, 49));
    }
    @Test
    public void statementCoverage() {
        assertFalse(verificare(0, new int[]{}, 0));
        assertFalse(verificare(1, new int[]{-4}, 0));
        assertTrue(verificare(1, new int[]{2}, 0));
    }
    @Test
    public void branchCoverage() {
        assertFalse(verificare(0, new int[]{}, 0));
        assertFalse(verificare(1, new int[]{}, -1));
        assertFalse(verificare(1, new int[]{1}, -1));
        assertTrue(verificare(1, new int[]{1}, 0));
        assertFalse(verificare(1, new int[]{1}, 1));
        assertFalse(verificare(2, new int[]{1}, 0));
    }
    @Test
    public void conditionCoverage() {
        assertFalse(verificare(0, new int[]{}, 0));
        int p[] = IntStream.rangeClosed(0, 50).map(x -> x = 1).toArray();
        assertFalse(verificare(51, new int[]{}, 0));
        assertFalse(verificare(1, new int[]{5}, -1));
        assertFalse(verificare(1, new int[]{5}, 1));
        assertFalse(verificare(2, new int[]{5}, 0));
        assertFalse(verificare(1, new int[]{-1}, 0));
        assertTrue(verificare(1, new int[]{1}, 0));
    }
    @Test
    public void pathCoverage() {
        assertFalse(verificare(0, new int[]{}, 0));
        assertFalse(verificare(1, new int[]{-1}, 0));
        assertTrue(verificare(1, new int[]{2}, 0));
    }
    @Test
    public void killChangedConditionalBoundary(){
        assertTrue(verificare(2, new int[]{0,0}, 0));
    }
}
