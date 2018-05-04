package main;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.store.FSDirectory;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.StringTokenizer;

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

            FileReader file = new FileReader("Docs/" + doc.get("path"));
            BufferedReader br = new BufferedReader(file);
            ArrayList<String> wordList = new ArrayList<>();
            String line;
            StringTokenizer st;
            while((line=br.readLine())!=null)
            {
                st = new StringTokenizer(line, " ");
                while(st.hasMoreTokens()){
                    wordList.add(st.nextToken());
                }
            }
            br.close();

            String word = query.toString().replaceFirst("contents:", "");

            //Cream obiecte de tipul QueryResult si le ordonam dupa score
            resultList.add(new QueryResult(doc.get("path"), hits[i].score, TFIDF.tf(wordList, word)));

            Collections.sort(resultList);
        }
        return resultList;
    }
}
