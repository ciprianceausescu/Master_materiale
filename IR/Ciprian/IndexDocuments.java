package main;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;

class IndexDocuments {
    static void index() throws IOException {
        FileUtils.cleanDirectory(new File("index"));
        Indexer indexer = new Indexer("index");
        indexer.createIndex("Docs/", new LuceneFileFilter());
        indexer.close();
    }
}
