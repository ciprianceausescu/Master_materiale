import java.sql.*;

/**
 * Created by Ciprian Mihai on 5/2/2018.
 */
public class Main {
    public static void main(String[] args) {
        try {
            Connection con = DBUtil.
                    getConnection(DBType.MYSQLDB);

            /*Selectam datele din tabel*/
            String sql_select = "select * from agenda";
            Statement stmt = con.createStatement();
            ResultSet rs = stmt.executeQuery(sql_select);

            while(rs.next())
                System.out.println(
                        rs.getString(1)+ " " +
                        rs.getString(2));

            /*Inseram date in tabel*/
            String sql_insert = "insert into agenda values('Popescu', 'Ion', '0212101010','0230123123', 'test@gmail.com', 'Test, nr 25', 'Bucuresti'," +
                    "'Bucuresti', '111111')";
            int result_insert = stmt.executeUpdate(sql_insert);
            if(result_insert>0)
                System.out.println("Insert cu succes!");
            else
                System.out.println("Insert fara succes!");

            /*UPDATE*/
            String sql_update = "update agenda set nume = 'Ionescu' " +
                    "where nume like 'Popescu'";
            int result_update = stmt.executeUpdate(sql_update);
            if(result_update>0)
                System.out.println("Update cu succes!");
            else
                System.out.println("Update fara succes!");

            /*delete*/
            String sql_delete = "delete from agenda where nume like 'Ionescu'";
            int result_delete = stmt.executeUpdate(sql_delete);
            if(result_delete>0)
                System.out.println("Delete cu succes!");
            else
                System.out.println("Delete fara succes!");

            /*Insert with batch*/
            /*for(int i=0;i<100;i++)
                stmt.addBatch(sql_insert);

            stmt.executeBatch();*/

            /*PreparedStatement*/
            String sql_insert2 = "insert into agenda " +
                    "values(?,?,?,?,?,?,?,?,?)";
            PreparedStatement pstm = con.prepareStatement(sql_insert2);
            pstm.setString(1, "Popa");
            pstm.setString(2, "Marin");
            pstm.setString(3, "123");
            pstm.setString(4, "321");
            pstm.setString(5, "Marin@marin.com");
            pstm.setString(6, "Adresa");
            pstm.setString(7, "Oras");
            pstm.setString(8, "Judet");
            pstm.setString(9, "123");

            pstm.execute();






        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}
