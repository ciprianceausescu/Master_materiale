public class Problem_1 {
    public static void main(String[] args) {
        int []v = new int[1000];
        int sum = 0;
        for(int i=1;i<1000;i++){
            if(i%3==0 || i%5==0)
                v[i] = 1;
            else
                v[i] = 0;
        }
        for(int i=1;i<1000;i++) {
            if(v[i]==1)
                sum+=i;
        }
        System.out.println(sum);
    }
}
