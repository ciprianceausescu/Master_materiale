package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;

import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.FieldType;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.poifs.filesystem.FileMagic;
import org.apache.poi.xwpf.extractor.XWPFWordExtractor;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.tika.Tika;
import org.jsoup.Jsoup;

import static org.apache.lucene.index.IndexOptions.DOCS_AND_FREQS_AND_POSITIONS;
//Clasa indexer realizează indexarea documentelor
public class LuceneCustomRomanianIndexer {
    //Obiectul care se va ocupa de scrierea index-ului
    private IndexWriter indexWriterObject;
    //Constructorul clasei care setează folderul în care se va scrie index-ul
    public LuceneCustomRomanianIndexer(String indexDirectoryPath) throws IOException {
        //Directorul în care se va scrie index-ul
        Directory indexDirectoryObject = FSDirectory.open(Paths.get(indexDirectoryPath));
        //Se va realiza un indexer pentru Limba Română
        IndexWriterConfig config = new IndexWriterConfig(new RomanianAnalyzer());
        //Se va instanția obiectul indexWriterObject, cu obiectul config instanțiat anterior, pentru Limba Română
        indexWriterObject = new IndexWriter(indexDirectoryObject, config);
    }

    //Metoda de închidere a index-ului
    public void close() throws IOException {
        indexWriterObject.close();
    }
    //Crearea efectivă a index-ului
    private Document getDocument(File file) throws IOException {
        Document document = new Document();
        Field fieldContents = null;
        String fileType = new Tika().detect(file);

        FieldType fieldType = new FieldType();
        fieldType.setStored(true);
        fieldType.setStoreTermVectors(true);
        fieldType.setIndexOptions(DOCS_AND_FREQS_AND_POSITIONS);
        fieldType.setStoreTermVectorPayloads(true);
        fieldType.setStoreTermVectorOffsets(true);
        fieldType.setStoreTermVectorPositions(true);

        if(fileType.contains("html")) {
            fieldContents = new Field(LuceneConstantsFields.CONTENTS,
                Jsoup.parse(file,null,"127.0.0.1").text(), fieldType);

        }
        else
            if(fileType.contains("pdf")) {
                fieldContents = new Field(LuceneConstantsFields.CONTENTS,
                    new PDFTextStripper().getText(PDDocument.load(file)), fieldType);

            }
            else
                if(fileType.contains("doc")){
                    BufferedInputStream wordInputStream = new BufferedInputStream(new FileInputStream(file.getCanonicalPath()));
                    String text = null;
                    if (FileMagic.valueOf(wordInputStream) == FileMagic.OLE2) {
                        WordExtractor ex = new WordExtractor(wordInputStream);
                        text = ex.getText();
                        ex.close();
                    }
                    else
                        if(FileMagic.valueOf(wordInputStream) == FileMagic.OOXML) {
                            XWPFDocument doc = new XWPFDocument(wordInputStream);
                            XWPFWordExtractor extractor = new XWPFWordExtractor(doc);
                            text = extractor.getText();
                            extractor.close();
                        }
                    fieldContents = new Field(LuceneConstantsFields.CONTENTS, text, fieldType);
                }
                else
                    if(fileType.contains("plain")) {
                        fieldContents = new Field(LuceneConstantsFields.CONTENTS,
                                new java.util.Scanner(file,"UTF-8").useDelimiter("\\A").next(), fieldType);
                    }
        Field fileNameField = new Field(LuceneConstantsFields.FILE_NAME,
            file.getName(),StoredField.TYPE);

        Field filePathField = new Field(LuceneConstantsFields.FILE_PATH,
            file.getCanonicalPath(),StoredField.TYPE);

        if(fieldContents!=null) {
            document.add(fieldContents);
            document.add(fileNameField);
            document.add(filePathField);
        }
        return document;
    }

    private void indexFile(File file) throws IOException {
        System.out.println("Indexing file from path: "+file.getCanonicalPath());
        String type = new Tika().detect(file);
        System.out.println("Type of indexed file: "+ type);
        Document document = getDocument(file);
        indexWriterObject.addDocument(document);
    }

    public int createIndex(String dataDirPath, LuceneFileFilter filter) throws IOException {

        File[] allFilesList = new File(dataDirPath).listFiles();
        int innerDocsVar=0;

        for (File file : allFilesList) {
            if(!file.isDirectory() && !file.isHidden() && file.exists() && file.canRead() && filter.accept(file)){
                indexFile(file);
            }
            else
                if(file.isDirectory() && file.exists() && file.canRead()){
                    innerDocsVar+=createIndex(file.getCanonicalPath(),filter);
                }
        }
        return innerDocsVar+ indexWriterObject.numDocs();
    }
}