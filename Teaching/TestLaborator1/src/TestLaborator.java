import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.function.Consumer;

public class TestLaborator {
	public static void main(String []args){
		String fileName = "C:\\Users\\Ciprian Mihai\\Desktop\\Teaching\\TestLaborator1\\"+"data_auto.txt";
		try {
			BufferedReader br = new BufferedReader(new FileReader(fileName));
			
			String line;
			List<Automobil> listaAutomobil = new ArrayList<>();
			
			while((line = br.readLine()) != null){
				StringTokenizer st = new StringTokenizer(line, " ");
				
				while(st.hasMoreTokens()){
					listaAutomobil.add(new Automobil(st.nextToken().toString(),
							Integer.parseInt(st.nextToken()),
							Integer.parseInt(st.nextToken())));
				}
			}
			listaAutomobil.forEach(name -> System.out.println(name));
		} catch (FileNotFoundException e) {
			System.out.println(e.getMessage());
			e.printStackTrace();
		} catch(IOException e){
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
	}
}
