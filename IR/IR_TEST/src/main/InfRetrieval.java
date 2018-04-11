package main;

import javax.imageio.ImageIO;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.SpringLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import org.apache.commons.io.FileUtils;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.tika.exception.TikaException;
import org.xml.sax.SAXException;

public class InfRetrieval {
  public static void main(String[] args) throws IOException {
    JFrame frame = new JFrame("Indexer");
    BufferedImage backgroundImage = ImageIO.read(new File("C:\\Users\\Ciprian Mihai\\Desktop\\Master_materiale\\IR\\IR_TEST\\src\\bgimg.jpg"));
    frame.setContentPane(new ImagePanel(backgroundImage));
    SpringLayout layout = new SpringLayout();
    frame.getContentPane().setLayout(layout);

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(400,265);

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

            IndexFiles.index();

            JOptionPane.showMessageDialog(null, "Index written successfully",
                "Index ", JOptionPane.INFORMATION_MESSAGE);
            scrollPane.setVisible(false);
        } catch (IOException ex){
          ex.printStackTrace();
        } catch (TikaException e1) {
          e1.printStackTrace();
        } catch (SAXException e1) {
          e1.printStackTrace();
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

            String[] result = SearchFiles.search(input);
            if(result.length!=0)
                model.addElement("\"" + input + "\" found in " + result.length + " documents: \n");
            for (int i=0;i<result.length;i++) {
                model.addElement(result[i]+"\n");
            }
            if(result.length==0)
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