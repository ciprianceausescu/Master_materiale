package stringuri;

/**
 * Un alt exemplu de lucru cu StringBuffer.
 */
public class StringBufferEx2 {

    public static void main(String[] args) {
        StringBuffer sb = new StringBuffer();
        sb.append("aba");
        System.out.println(sb);
        StringBuffer rev = sb.reverse();
        System.out.println(rev);

        System.out.println(sb.equals(rev));

        StringBuffer x = new StringBuffer("abc");
        StringBuffer y = new StringBuffer("abc");
        System.out.println(x == y);
        System.out.println(x.equals(y));


    }

}