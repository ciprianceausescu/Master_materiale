/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

import org.apache.commons.io.FileUtils;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexNotFoundException;

public class InfRetrieval {
  private JPanel mainPanel;

  public static void main(String[] args) throws IOException {
    JFrame frame = new JFrame("Indexer");
    BufferedImage backgroundImage = ImageIO.read(new File("/home/adriana/Documents/lucene-solr-master/IndexerFf/src/background.jpg"));
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
          JFileChooser fileopen = new JFileChooser();
          fileopen.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

          int ret = fileopen.showDialog(null, "Choose file");

          if (ret == JFileChooser.APPROVE_OPTION) {

            FileUtils.cleanDirectory(new File("IndexDir"));

            Indexer indexer = new Indexer("IndexDir");
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

          Searcher searcher = new Searcher("IndexDir",input);
          ArrayList<Document> results = searcher.getResultedDocuments();
          for (Document result : results) {
            model.addElement(result.get(LuceneConstants.FILE_NAME)+"\n");
            //result.get(LuceneConstants.CONTENTS)+"\n\n");
          }
          if(results.size()==0) JOptionPane.showMessageDialog(null, "There were no results found.",
              "Results not found ", JOptionPane.INFORMATION_MESSAGE);
          else  scrollPane.setVisible(true);
          searcher.close();
        }  catch (IndexNotFoundException e1) {
          JOptionPane.showMessageDialog(null, "In folderul IndexDir nu s-a gasit index.",
              "Index not found ", JOptionPane.INFORMATION_MESSAGE);
        }catch (IOException e1) {
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
    //frame.pack();
    frame.setVisible(true);
  }
}