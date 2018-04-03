public class Problem_2 {
    public static void main(String[] args) {
        int e1=1, e2=2, e3 = 0;
        int sum = 2;
        while(e3<4000000){
            e3 = e1 + e2;
            if(e3%2==0)
                sum += e3;
            e1 = e2;
            e2 = e3;
        }
        System.out.println(sum);
    }
}
