import static org.junit.Assert.*;
import java.util.Arrays;
import org.junit.Test;

public class ArmstrongTests {
    @Test
    public void equivalencePartitioning() {
        int []a = {10, 4, 5, 7, 150};
        int []b = {1,1,1,1,1,1,1,1,1,1,1};
        assertEquals(16, Armstrong.solve(5, a, 1, 4));
        assertEquals(-1, Armstrong.solve(5, a, 1, -1));
        assertEquals(-1, Armstrong.solve(5, a, 1, 6));
        assertEquals(-1, Armstrong.solve(5, a, -1, -1));
        assertEquals(-1, Armstrong.solve(5, a, -1, 6));
        assertEquals(-1, Armstrong.solve(5, a, 6, 6));
        assertEquals(-1, Armstrong.solve(5, a, 10, 40));
        assertEquals(-1, Armstrong.solve(0, null, 1, 4));
        assertEquals(-1, Armstrong.solve(11, b, 1, 4));
    }

    @Test
    public void boundaryValueAnalysis() {
        int[]c = new int[10];
        Arrays.fill(c, 1);

        int []d = {153};
        int []e= {-1};
        assertEquals(-1, Armstrong.solve(1, d, -1, -1));
        assertEquals(-1, Armstrong.solve(1, d, -1, 0));
        assertEquals(-1, Armstrong.solve(10, c, -1, 9));
        assertEquals(-1, Armstrong.solve(1, d, -1, 1));
        assertEquals(153, Armstrong.solve(1, d, 0, 0));
        assertEquals(10, Armstrong.solve(10, c, 0, 9));
        assertEquals(1, Armstrong.solve(10, c, 9, 9));
        assertEquals(-1, Armstrong.solve(10, c, 10, 10));
        assertEquals(-1, Armstrong.solve(1, e, 0, 0));
        assertEquals(-1, Armstrong.solve(0, null, 1, 2));
        assertEquals(-1, Armstrong.solve(11, null, 0, 4));
    }

    @Test
    public void categoryPartitioning() {
        assertTrue(Armstrong.isArmstrongNumber(153));
        assertTrue(Armstrong.isArmstrongNumber(370));
        assertTrue(Armstrong.isArmstrongNumber(371));
        assertFalse(Armstrong.isArmstrongNumber(31));
        assertFalse(Armstrong.isArmstrongNumber(90));
        assertFalse(Armstrong.isArmstrongNumber(81));

        int a[] = {153};
        assertEquals(-1, Armstrong.solve(-1, null, 0, 0));
        assertEquals(-1, Armstrong.solve(0, null, 0, 0));
        assertEquals(153, Armstrong.solve(1, a, 0, 0));
        assertEquals(-1, Armstrong.solve(1, a, -1, 0));
        assertEquals(-1, Armstrong.solve(1, a, 3, 0));
        assertEquals(-1, Armstrong.solve(1, a, 0, -1));

        int[]b = {30, 40, 51, 153, 150};
        assertEquals(153, Armstrong.solve(5, b, 3, 4));
        assertEquals(-1, Armstrong.solve(5, b, 3, -1));
        assertEquals(-1, Armstrong.solve(5, b, 3, 5));
        assertEquals(-1, Armstrong.solve(5, b, 3, 6));

        int[]c = new int[10];
        Arrays.fill(c, 1);
        assertEquals(10, Armstrong.solve(10, c, 0, 9));
        assertEquals(-1, Armstrong.solve(11, c, 0, 9));
        assertEquals(-1, Armstrong.solve(10, c, 0, 11));
        assertEquals(1, Armstrong.solve(10, c, 9, 9));

        int[]d = {3, -153, -9};
        assertEquals(-1, Armstrong.solve(3, d, 0, 0));

        int[]e = new int[10];
        Arrays.fill(e, -1);
        assertEquals(-1, Armstrong.solve(10, e, 0, 0));
    }

    @Test
    public void statementCoverage() {
        assertEquals(-1, Armstrong.solve(0, null, 0, 0));

        int[]b = {3, 153, -5, -153, 370, 407};
        assertEquals(-1, Armstrong.solve(6, b, 0, 4));

        int[]c = {153, 51, 50, 82, 370};
        assertEquals(523, Armstrong.solve(5, c, 0, 4));
        assertEquals(153, Armstrong.solve(5, c, 0, 0));
    }

    @Test
    public void branchCoverage() {

        assertEquals(-1, Armstrong.solve(0, null, 0, 0));

        int[]a = {153, 91, 78, 71, 1};
        assertEquals(-1, Armstrong.solve(5, a, 6, 3));
        assertEquals(-1, Armstrong.solve(5, a, 2, -2));
        assertEquals(-1, Armstrong.solve(5, a, 2, 10));
        assertEquals(153, Armstrong.solve(5, a, 0, 1));

        int[]b = {153, -51, -153, 82, 370};
        assertEquals(-1, Armstrong.solve(5, b, 2, 4));

        int[]c = {153};
        assertEquals(153, Armstrong.solve(1, c, 0, 0));

        int[]d = {91};
        assertEquals(0, Armstrong.solve(1, d, 0, 0));
    }

    @Test
    public void conditionCoverage() {
        assertEquals(-1, Armstrong.solve(0, null, 0, 0));
        assertEquals(-1, Armstrong.solve(11, null, 0, 0));

        int[] a = {153, 91, 78};
        assertEquals(-1, Armstrong.solve(3, a, -5, 0));
        assertEquals(-1, Armstrong.solve(3, a, 10, 2));
        assertEquals(-1, Armstrong.solve(3, a, 2, -5));
        assertEquals(-1, Armstrong.solve(3, a, 2, 10));

        int[] b = {-1};
        assertEquals(-1, Armstrong.solve(0, b, 0, 0));

        int[] c = {31};
        assertEquals(0, Armstrong.solve(1, c, 0, 0));

        int[] d = {153};
        assertEquals(153, Armstrong.solve(1, d, 0, 0));

        int[] e = {21, 153, 370};
        assertEquals(523, Armstrong.solve(3, e, 1, 2));
    }

    @Test
    public void modifiedConditionDecision() {
        int[] d = {153};
        assertEquals(153, Armstrong.solve(1, d, 0, 0));
        assertEquals(-1, Armstrong.solve(-1, d, 0, 0));
        assertEquals(-1, Armstrong.solve(11, d, 0, 0));
        assertEquals(-1, Armstrong.solve(1, null, 0, 0));
        assertEquals(-1, Armstrong.solve(1, d, -1, 0));
        assertEquals(-1, Armstrong.solve(1, d, 1, 0));
        assertEquals(-1, Armstrong.solve(1, d, 0, -1));
        assertEquals(-1, Armstrong.solve(1, d, 0, 1));
    }

    @Test
    public void pathCoverage() {
        assertEquals(-1, Armstrong.solve(0, null, 0, 0));

        int[]a = {-1, 1, 0};
        assertEquals(-1, Armstrong.solve(3, a, 0, 1));

        int[]b = {155, 91, 78};
        assertEquals(0, Armstrong.solve(3, b, 0, 1));

        int[]c = {155, 91, 153};
        assertEquals(153, Armstrong.solve(3, c, 0, 2));
    }

    @Test
    public void Mutant() {
        int []d = {0,1,2};
        assertEquals(0, Armstrong.solve(1,d, 0, 0));
        int[]c = {1, 21, 34, 153, 56};
        assertEquals(-1, Armstrong.solve(5, c, 5, 0));
    }
}
