package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import javax.imageio.ImageIO;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JFileChooser;
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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.search.highlight.InvalidTokenOffsetsException;
//Clasă principală care crează Form-ul și oferă acces la funcționalitățile proiectului
public class InformationRetrievalForm {
    public static void main(String[] args) throws IOException {
        //Crearea Frame-ului
        JFrame frameObject = new JFrame("Lucene Custom Romanian Indexer - Ciprian Mihai Ceaușescu");
        //Imaginea de fundal a interfeței grafice
        BufferedImage bufferedImageObject = ImageIO.read(new File("C:\\Users\\Ciprian Mihai\\Desktop\\Master_materiale\\IR\\IR_TEST\\src\\img.png"));
        frameObject.setContentPane(new ImagePanel(bufferedImageObject));
        SpringLayout springLayoutObject = new SpringLayout();
        frameObject.getContentPane().setLayout(springLayoutObject);

        frameObject.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frameObject.setSize(600,400);

        //Centrarea interfeței grafice în funcție de dimensiunile ecranului echipamentului
        Dimension dimensionObject = Toolkit.getDefaultToolkit().getScreenSize();
        int x = (int) ((dimensionObject.getWidth()-frameObject.getWidth())/2);
        int y = (int) ((dimensionObject.getHeight()-frameObject.getHeight())/2);
        frameObject.setLocation(x,y);

        JButton searchButtonObject = new JButton("Search");
        searchButtonObject.setBounds(159, 94, 121, 23);

        DefaultListModel<String> defaultListModelObject = new DefaultListModel<>();
        JList<String> listObject = new JList<>(defaultListModelObject);
        JScrollPane scrollPaneObject = new JScrollPane();
        scrollPaneObject.setViewportView(listObject);
        scrollPaneObject.setPreferredSize(new Dimension(500,300));

        JButton openFileButtonObject = new JButton("Index");
        openFileButtonObject.setBounds(159, 94, 121, 23);
        openFileButtonObject.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    JFileChooser jFileChooserObject = new JFileChooser();
                    jFileChooserObject.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

                    int jFileChooserReturnValue = jFileChooserObject.showDialog(null, "Choose file");

                    if (jFileChooserReturnValue == JFileChooser.APPROVE_OPTION) {
                        String fileName = "index";
                        //Se șterge index-ul vechi
                        FileUtils.cleanDirectory(new File(fileName));
                        //Se instanțiază un obiect indexer
                        LuceneCustomRomanianIndexer luceneCustomRomanianIndexerObject = new LuceneCustomRomanianIndexer(fileName);
                        //Se setează constanta FILES_PATH ca fiind valoarea căii care a fost selectată
                        LuceneConstantsFields.FILES_PATH = jFileChooserObject.getSelectedFile().toString();
                        //Se crează index-ul folosind fișierele de la calea care a fost selectată
                        luceneCustomRomanianIndexerObject.createIndex(jFileChooserObject.getSelectedFile().toString(), new LuceneFileFilter());
                        //Se închide index-ul, salvându-se toate fișierele în directorul index
                        luceneCustomRomanianIndexerObject.close();
                        //Se afișează un mesaj de succes
                        JOptionPane.showMessageDialog(null, "Index written successfully into "
                                        + fileName + " directory.",
                        "Index successfully", JOptionPane.INFORMATION_MESSAGE);

                        scrollPaneObject.setVisible(false);
                    }
                }
                catch (IOException ex){
                    ex.printStackTrace();
                }
            }
        });

        //Acțiune realizată la apăsarea pe butonul de Search
    searchButtonObject.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                //Se șterg rezultatele din fereastra defaultListModelObject
                defaultListModelObject.removeAllElements();
                //Se deschide fereastra în care vom introduce interogarea
                String input = JOptionPane.showInputDialog("Enter search query.","");
                //Dacă nu se introduce nimic în JOptionPane, se iese din metodă fără mesaj de eroare
                if(input==null) return;
                //Dacă s-a introdus o interogare, se instanțiază obiectul luceneCustomRomanianSearcher care caută prin documentele indexate
                //cuvintele din interogarea realizată
                LuceneCustomRomanianSearcher luceneCustomRomanianSearcher = new LuceneCustomRomanianSearcher("index",input);
                //Se preiau rezultatele folosind metoda getResultedDocuments()
                ArrayList<DocResults> docResults = luceneCustomRomanianSearcher.getResultedDocuments();
                //Se determină statusul pentru fiecare cuvânt din interogare
                List<QueryStatuses> queryStatusesList = luceneCustomRomanianSearcher.statusQuery();
                //Se afișează numărul de documente rezultate în urma interogării realizate
                defaultListModelObject.addElement("Found " + docResults.size() + "hits.");
                defaultListModelObject.addElement("\n");
                //Se afisează informațiile despre fiecare cuvânt din interogare
                for (QueryStatuses queryStatObject : queryStatusesList) {
                    defaultListModelObject.addElement("Token information: " + queryStatObject.toString());
                }
                defaultListModelObject.addElement("\n");

                //Se afisează informațiile despre documente în interfața grafică, și anume numele documentului
                //și restul informațiilor utile
                for (DocResults docResult : docResults) {
                    defaultListModelObject.addElement("Document name: ");
                    defaultListModelObject.addElement(docResult.toString());
                    //result.get(LuceneConstantsFields.CONTENTS)+"\n\n");
                }
                //Dacă nu s-au găsit rezultate, adică dimensiunea listei este 0, se afișează un JOptionPane de informare
                //prin care utilizatorul este informat că nu s-au găsit rezultate pentru interogarea realizată
                if(docResults.size()==0)
                    JOptionPane.showMessageDialog(null, "There were no results found.",
                "Results not found", JOptionPane.INFORMATION_MESSAGE);
                else//Dacă s-au găsit rezultate, se afișează toate aceste rezultate care s-au parcurs anterior
                    scrollPaneObject.setVisible(true);
                luceneCustomRomanianSearcher.close();
        }
        //Se tratează diversele erori apărute
        catch (IndexNotFoundException e1) {
            JOptionPane.showMessageDialog(null, "In directory index there is not 'index' file.",
              "Index not found", JOptionPane.INFORMATION_MESSAGE);
        }
        catch (IOException e1) {
            e1.printStackTrace();
        }
        catch (InvalidTokenOffsetsException e1) {
            e1.printStackTrace();
        }
        }
    });

    frameObject.getContentPane().add(openFileButtonObject);
    frameObject.getContentPane().add(searchButtonObject);

    frameObject.getContentPane().add(scrollPaneObject);
    springLayoutObject.putConstraint(SpringLayout.WEST,openFileButtonObject,5,SpringLayout.EAST,searchButtonObject);
    springLayoutObject.putConstraint(SpringLayout.WEST,searchButtonObject,210, SpringLayout.WEST,frameObject.getContentPane());
    springLayoutObject.putConstraint(SpringLayout.NORTH,searchButtonObject,20, SpringLayout.NORTH,frameObject.getContentPane());
    springLayoutObject.putConstraint(SpringLayout.NORTH,openFileButtonObject,20, SpringLayout.NORTH,frameObject.getContentPane());
    springLayoutObject.putConstraint(SpringLayout.NORTH,scrollPaneObject,50, SpringLayout.NORTH,frameObject.getContentPane());
    springLayoutObject.putConstraint(SpringLayout.WEST,scrollPaneObject,50, SpringLayout.WEST,frameObject.getContentPane());

    scrollPaneObject.setVisible(false);
    frameObject.setVisible(true);
    }
}