
package Stiva;


import java.util.Random;

public class TestStiva
{
    public static void main(String[] args)
    {
        StivaObiecte st = new StivaObiecte(3);
      try{
      
      st.push(new Integer(1));
      st.push(new Integer(2));
      //st.push(new Integer(3));
      //st.push(new Integer(4));
        st.pop();
        st.pop();
        st.pop();
        st.afisareStiva();
       }catch(ExceptieStiva a)
        {
            System.err.println(a.getMessage());
           // a.printStackTrace();
        }
    }
    
}


