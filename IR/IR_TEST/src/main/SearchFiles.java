package main;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.FieldInvertState;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;

import static java.lang.Math.log;

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
    static ArrayList<QueryResult> searchList(String word) throws Exception {
        String indexFilesPath = "index";
        IndexReader indexReader = DirectoryReader.open(FSDirectory.open(Paths.get(indexFilesPath)));
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));

        IndexSearcher searcher = new IndexSearcher(indexReader);
        Analyzer analyzer = new RoAnalyzer();
        QueryParser parser = new QueryParser("contents", analyzer);
        Query query = parser.parse(word);
        return resultList(searcher, query);
    }
    private static String[] result(IndexSearcher searcher, Query query)
            throws IOException {

        TopDocs topDocsResult = searcher.search(query, LuceneConstants.MAX_HITS);
        ScoreDoc[] hits = topDocsResult.scoreDocs;

        int numTotalHits = topDocsResult.totalHits;
        String []res = new String[numTotalHits];

        ArrayList<QueryResult> resultList = new ArrayList<QueryResult>();
        for (int i = 0; i < numTotalHits; i++) {
            Document doc = searcher.doc(hits[i].doc);
            String path = doc.get("path");
            res[i] = path;
        }
        return res;
    }
    private static ArrayList<QueryResult> resultList(IndexSearcher searcher, Query query)
            throws IOException {

        TopDocs topDocsResult = searcher.search(query, LuceneConstants.MAX_HITS);
        ScoreDoc[] hits = topDocsResult.scoreDocs;

        int numTotalHits = topDocsResult.totalHits;
        String []res = new String[numTotalHits];

        ArrayList<QueryResult> resultList = new ArrayList<QueryResult>();
        for (int i = 0; i < numTotalHits; i++) {
            Document doc = searcher.doc(hits[i].doc);

            //Cream obiecte de tipul QueryResult si le ordonam dupa score

            resultList.add(new QueryResult(doc.get("path"), hits[i].score));
            Collections.sort(resultList);
        }
        return resultList;
    }
}

class Test extends TFIDFSimilarity{

    @Override
    public float coord(int i, int i1) {
        return 0;
    }

    @Override
    public float queryNorm(float v) {
        return 0;
    }

    @Override
    public float tf(float v) {
        return 0;
    }

    @Override
    public float idf(long l, long l1) {
        return  (float)log(l1/(l+1)) + 1;
    }

    @Override
    public float lengthNorm(FieldInvertState fieldInvertState) {
        return 0;
    }

    @Override
    public float decodeNormValue(long l) {
        return 0;
    }

    @Override
    public long encodeNormValue(float v) {
        return 0;
    }

    @Override
    public float sloppyFreq(int i) {
        return 0;
    }

    @Override
    public float scorePayload(int i, int i1, int i2, BytesRef bytesRef) {
        return 0;
    }
}