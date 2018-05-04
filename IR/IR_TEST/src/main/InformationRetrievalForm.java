package main;

import javax.imageio.ImageIO;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.SpringLayout;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;

import org.apache.commons.io.FileUtils;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

public class InformationRetrievalForm {
  public static void main(String[] args) throws IOException {
    JFrame frame = new JFrame("Indexer");
    BufferedImage backgroundImage = ImageIO.read(new File("C:\\Users\\Ciprian Mihai\\Desktop\\Master_materiale\\IR\\IR_TEST\\src\\bgimg.jpg"));
    frame.setContentPane(new ImagePanel(backgroundImage));

    SpringLayout layout = new SpringLayout();
    frame.getContentPane().setLayout(layout);

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(400,265);

    Dimension dimension = Toolkit.getDefaultToolkit().getScreenSize();
    int x = (int) ((dimension.getWidth()-frame.getWidth())/2);
    int y = (int) ((dimension.getHeight()-frame.getHeight())/2);
    frame.setLocation(x,y);

    JButton searchButton = new JButton("Search");
    searchButton.setBounds(159, 94, 121, 23);

    DefaultListModel<String> model = new DefaultListModel<>();
    JList<String> list = new JList<>(model);
    JScrollPane scrollPane = new JScrollPane();
    scrollPane.setViewportView(list);
    scrollPane.setPreferredSize(new Dimension(300,170));

    JButton openFileButton = new JButton("Index");
    openFileButton.setBounds(159, 94, 121, 23);
    openFileButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
            FileUtils.cleanDirectory(new File("index"));

            IndexDocuments.index();

            JOptionPane.showMessageDialog(null, "Index written successfully",
                "Index ", JOptionPane.INFORMATION_MESSAGE);
            scrollPane.setVisible(false);
        } catch (IOException ex){
          ex.printStackTrace();
        }
      }
    });

    searchButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        try {
            model.removeAllElements();

            String input = JOptionPane.showInputDialog("Enter search query.","");
            if(input==null) return;

            /*String[] result = SearchFiles.search(input);
            if(result.length!=0) {
                model.addElement("\"" + input + "\" found in " + result.length + " documents: \n");
                System.out.println("\n\"" + input + "\" found in " + result.length + " documents:");
            }
            for (int i=0;i<result.length;i++) {
                model.addElement(result[i]+"\n");
                System.out.println(result[i]);
            }
            if(result.length==0)
                JOptionPane.showMessageDialog(null, "There were no results found.",
                  "Results not found ", JOptionPane.INFORMATION_MESSAGE);
            else
                scrollPane.setVisible(true);*/

            ArrayList<QueryResult> resultList = SearchFiles.searchList(input);

            if(resultList.size()!=0) {
                Test test = new Test();

                IndexReader reader = null;
                Directory dir = FSDirectory.open(Paths.get("index"));
                reader = DirectoryReader.open(dir);
                int docs = reader.numDocs();

                float idf = test.idf(resultList.size(), docs);
                DecimalFormat df = new DecimalFormat();
                df.setMaximumFractionDigits(2);

                model.addElement("IDF: " + df.format(idf) + "\n");
                model.addElement("\"" + input + "\" found in " + resultList.size() + " documents: \n");
                System.out.println("\nIDF: " + df.format(idf));
                System.out.println(input + "\" found in " + resultList.size() + " documents:");
            }
            for (int i=0;i<resultList.size();i++) {
                model.addElement(resultList.get(i) + "\n");
                System.out.println(resultList.get(i));
            }
            if(resultList.size()==0)
                JOptionPane.showMessageDialog(null, "There were no results found.",
                        "Results not found ", JOptionPane.INFORMATION_MESSAGE);
            else
                scrollPane.setVisible(true);

        }  catch (IndexNotFoundException e1) {
          JOptionPane.showMessageDialog(null, "In folderul IndexDir nu s-a gasit index.",
              "Index not found ", JOptionPane.INFORMATION_MESSAGE);
        }catch (IOException e1) {
            e1.printStackTrace();
        } catch (Exception e1) {
            e1.printStackTrace();
        }
      }
    });

    frame.getContentPane().add(openFileButton);
    frame.getContentPane().add(searchButton);

    frame.getContentPane().add(scrollPane);
    layout.putConstraint(SpringLayout.WEST,openFileButton,5,SpringLayout.EAST,searchButton);
    layout.putConstraint(SpringLayout.WEST,searchButton,110, SpringLayout.WEST,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,searchButton,20, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,openFileButton,20, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,scrollPane,50, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.WEST,scrollPane,50, SpringLayout.WEST,frame.getContentPane());

    scrollPane.setVisible(false);
    frame.setVisible(true);
  }
}