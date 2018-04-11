package main;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.FSDirectory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;

/**
 * Simple command-line based search demo.
 */
class SearchFiles {
    static String[] search(String word) throws Exception {
        String indexFilesPath = "index";
        IndexReader indexReader = DirectoryReader.open(FSDirectory.open(Paths.get(indexFilesPath)));
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));

        IndexSearcher searcher = new IndexSearcher(indexReader);
        Analyzer analyzer = new RoAnalyzer();
        QueryParser parser = new QueryParser("contents", analyzer);
        Query query = parser.parse(word);
        return result(searcher, query);
    }
    private static String[] result(IndexSearcher searcher, Query query)
            throws IOException {
        TopDocs topDocsResult = searcher.search(query, LuceneConstants.MAX_HITS);
        ScoreDoc[] hits = topDocsResult.scoreDocs;

        int numTotalHits = topDocsResult.totalHits;
        String []res = new String[numTotalHits];

        for (int i = 0; i < numTotalHits; i++) {
            Document doc = searcher.doc(hits[i].doc);
            String path = doc.get("path");
            res[i] = path;
        }
        return res;
    }
}