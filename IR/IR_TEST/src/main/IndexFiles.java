package main;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.*;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.IndexWriterConfig.OpenMode;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.Parser;
import org.apache.tika.sax.BodyContentHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Date;

import static org.apache.lucene.index.IndexOptions.DOCS_AND_FREQS_AND_POSITIONS;

public class IndexFiles {

    /**
     * Clasa IndexFiles indexează fișierele dintr-o anumită locație
     */
    static void index() throws TikaException, SAXException, IOException {
        //indexPath = locatia unde se va salva index-ul creat
        String indexPath = "index";
        //docsPath = locatia unde se găsesc fișierele care se vor indexa
        String docsPath = "Docs/";

        final Path docDir = Paths.get(docsPath);
        if (!Files.isReadable(docDir)) {
            System.out.println("Document directory '" + docDir.toAbsolutePath() + "' does not exist or is not readable, please check the path");
            System.exit(1);
        }

        Date start = new Date();
        System.out.println("Indexing to directory '" + indexPath + "'...");

        Directory dir = FSDirectory.open(Paths.get(indexPath));
        Analyzer analyzer = new RoAnalyzer();

        IndexWriterConfig iwc = new IndexWriterConfig(analyzer);
        iwc.setOpenMode(OpenMode.CREATE);

        IndexWriter writer = new IndexWriter(dir, iwc);
        indexDocs(writer, docDir);

        writer.close();

        System.out.println("Duration: " + (new Date().getTime() - start.getTime()) / 1000 + " seconds");
    }

    private static void indexDocs(final IndexWriter writer, Path path)
            throws IOException, TikaException, SAXException {
        if (Files.isDirectory(path)) {
            Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                    try {
                        indexDoc(writer, file);
                    } catch (Exception ignore) {
                        ignore.printStackTrace();
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } else {
            indexDoc(writer, path);
        }
    }
    /**
     * Indexes a single document
     */
    private static void indexDoc(IndexWriter writer, Path path)
            throws IOException, TikaException, SAXException {

        FieldType fieldType = new FieldType();
        fieldType.setStored(true);
        fieldType.setStoreTermVectors(true);
        fieldType.setIndexOptions(DOCS_AND_FREQS_AND_POSITIONS);

        File file = new File(path.toString());

        Document doc = new Document();

        Field pathField = new StringField("path", path.toString(), Field.Store.YES);
        doc.add(new TextField("contents", new BufferedReader(new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8))));
        doc.add(pathField);

        ContentHandler handler = new BodyContentHandler();
        FileInputStream is = new FileInputStream(file);

        Metadata metadata = new Metadata();
        metadata.set(Metadata.RESOURCE_NAME_KEY, file.getCanonicalPath());

        Parser parser = new AutoDetectParser();
        ParseContext context = new ParseContext();
        parser.parse(is, handler, metadata, context);

        System.out.println("added " + path);
        writer.addDocument(doc);
    }
}