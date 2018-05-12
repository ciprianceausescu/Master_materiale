package main;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;

import org.apache.lucene.document.*;
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

class Indexer {

    private IndexWriter indexWriter;

    Indexer(String indexDirectoryPath) throws IOException {
        Directory indexDirectory = FSDirectory.open(Paths.get(indexDirectoryPath));
        IndexWriterConfig indexWriterConfig = new IndexWriterConfig(new RoAnalyzer());
        indexWriter = new IndexWriter(indexDirectory, indexWriterConfig);
    }

    void close() throws IOException {
        indexWriter.close();
    }

    private Document getDocument(File file) throws IOException {
        Document document = new Document();
        Field field = null;
        String detect = new Tika().detect(file);

        if (detect.contains("html")) {
            field = new Field(LuceneConstants.CONTENTS,
                    Jsoup.parse(file, null, "127.0.0.1").text(),
                    TextField.TYPE_STORED);
        }
        else if (detect.contains("pdf")) {
            field = new Field(LuceneConstants.CONTENTS,
                    new PDFTextStripper().getText(PDDocument.load(file)),
                    TextField.TYPE_STORED);

        }
        else if (detect.contains("doc")) {
            BufferedInputStream wordInputStream = new BufferedInputStream(new FileInputStream(file.getCanonicalPath()));
            String text = null;
            if (FileMagic.valueOf(wordInputStream) == FileMagic.OLE2) {
                WordExtractor ex = new WordExtractor(wordInputStream);
                text = ex.getText();
                ex.close();
            }
            else if (FileMagic.valueOf(wordInputStream) == FileMagic.OOXML) {
                XWPFDocument doc = new XWPFDocument(wordInputStream);
                XWPFWordExtractor extractor = new XWPFWordExtractor(doc);
                text = extractor.getText();
                extractor.close();
            }
            field = new Field(LuceneConstants.CONTENTS, text,
                    TextField.TYPE_STORED);

        }
        else if (detect.contains("plain")) {
            field = new Field(LuceneConstants.CONTENTS,
                    new java.util.Scanner(file, "UTF-8").useDelimiter("\\A").next(),
                    TextField.TYPE_STORED);
        }
        if (field != null) {
            Field pathField = new StringField("path", file.getName(), Field.Store.YES);
            document.add(field);
            document.add(pathField);
        }
        return document;
    }

    private void indexFile(File file) throws IOException {
        System.out.println("Indexing file: " + file.getCanonicalPath());
        String type = new Tika().detect(file);
        System.out.println("Type of file: " + type);
        Document document = getDocument(file);
        indexWriter.addDocument(document);
    }

    int createIndex(String dataDirPath, LuceneFileFilter filter)
            throws IOException {
        File[] files = new File(dataDirPath).listFiles();
        int innerDocs = 0;

        for (File file : files) {
            if (!file.isDirectory() && !file.isHidden() && file.exists() && file.canRead() && filter.accept(file)) {
                indexFile(file);
            }
            else if (file.isDirectory() && file.exists() && file.canRead()) {
                innerDocs += createIndex(file.getCanonicalPath(), filter);
            }
        }
        return innerDocs + indexWriter.numDocs();
    }
}