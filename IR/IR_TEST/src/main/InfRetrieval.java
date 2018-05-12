package main;

import javax.imageio.ImageIO;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SpringLayout;
import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.ScrollPane;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.search.highlight.InvalidTokenOffsetsException;

public class InfRetrieval {
  private JPanel mainPanel;

  public static void main(String[] args) throws IOException {
    JFrame frame = new JFrame("Indexer");
    BufferedImage backgroundImage = ImageIO.read(new File("C:\\Users\\Ciprian Mihai\\Desktop\\Master_materiale\\IR\\IR_TEST\\src\\img.png"));
    frame.setContentPane(new ImagePanel(backgroundImage));
    SpringLayout layout = new SpringLayout();
    frame.getContentPane().setLayout(layout);

    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(600,400);

    JButton searchButton = new JButton("Search");
    searchButton.setBounds(159, 94, 121, 23);

    DefaultListModel<String> model = new DefaultListModel<>();
    JList<String> list = new JList<>(model);
    JScrollPane scrollPane = new JScrollPane();
    scrollPane.setViewportView(list);
    scrollPane.setPreferredSize(new Dimension(500,300));

    JButton openFileButton = new JButton("Index");
    openFileButton.setBounds(159, 94, 121, 23);
    openFileButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          JFileChooser fileopen = new JFileChooser();
          fileopen.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

          int ret = fileopen.showDialog(null, "Choose file");

          if (ret == JFileChooser.APPROVE_OPTION) {

            FileUtils.cleanDirectory(new File("index"));

            Indexer indexer = new Indexer("index");
            indexer.createIndex(fileopen.getSelectedFile().toString(), new LuceneFileFilter());
            indexer.close();

            JOptionPane.showMessageDialog(null, "Index written successfully",
                "Index ", JOptionPane.INFORMATION_MESSAGE);
            scrollPane.setVisible(false);
          }
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

          Searcher searcher = new Searcher("index",input);
          ArrayList<DocResults> results = searcher.getResultedDocuments();
          List<QueryStats> queryStats = searcher.statusQuery();
          //afisare stat query
          for (QueryStats queryStat : queryStats) {
            model.addElement(queryStat.toString());
          }

          //afisare rezultate
          for (DocResults result : results) {
            model.addElement(result.toString());
            //result.get(LuceneConstants.CONTENTS)+"\n\n");
          }
          if(results.size()==0) JOptionPane.showMessageDialog(null, "There were no results found.",
              "Results not found ", JOptionPane.INFORMATION_MESSAGE);
          else  scrollPane.setVisible(true);
          searcher.close();
        }  catch (IndexNotFoundException e1) {
          JOptionPane.showMessageDialog(null, "In folderul index nu s-a gasit index.",
              "Index not found ", JOptionPane.INFORMATION_MESSAGE);
        }catch (IOException e1) {
          e1.printStackTrace();
        } catch (InvalidTokenOffsetsException e1) {
          e1.printStackTrace();
        }

      }
    });

    frame.getContentPane().add(openFileButton);
    frame.getContentPane().add(searchButton);

    frame.getContentPane().add(scrollPane);
    layout.putConstraint(SpringLayout.WEST,openFileButton,5,SpringLayout.EAST,searchButton);
    layout.putConstraint(SpringLayout.WEST,searchButton,210, SpringLayout.WEST,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,searchButton,20, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,openFileButton,20, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.NORTH,scrollPane,50, SpringLayout.NORTH,frame.getContentPane());
    layout.putConstraint(SpringLayout.WEST,scrollPane,50, SpringLayout.WEST,frame.getContentPane());

    scrollPane.setVisible(false);
    //frame.pack();
    frame.setVisible(true);
  }
}