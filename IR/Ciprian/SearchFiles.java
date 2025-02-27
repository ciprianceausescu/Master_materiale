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
import java.util.List;
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
        ArrayList<DocResults> docResults = new ArrayList<DocResults>();
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

            /******************************************************************************************/


            docResults.add(new DocResults(hits[i].score, i, doc.get("path")));

            //statusQuery(query, docResults, searcher);
        }
        return resultList;
    }
    /*public static List<QueryStats> statusQuery(Query query, ArrayList<DocResults> results, IndexReader indexReader){
        if(query==null) return null;

        List<QueryStats> stats = new ArrayList<>();

        List<BooleanClause> clauses = null;
        if(query instanceof BooleanQuery)
        {
            clauses = ((BooleanQuery) query).clauses();
        }

        if(clauses == null){
            QueryStats newStat = new QueryStats();
            newStat.token = query.toString(LuceneConstants.CONTENTS);
            System.out.println(newStat.token);
            newStat.tfq = 1;
            newStat.df = 0;

            if(results!=null) {
                for (DocResults result : results) {
                    if(result.hasTerm(newStat.token)){
                        newStat.df++;
                    }
                }
            }

            if(newStat.idf>0) newStat.idf = (float)Math.log(indexReader.numDocs()/newStat.df);

            stats.add(newStat);

        }else {
            List<BooleanClause> uniqueClauses = new ArrayList<>();
            for (BooleanClause clause : clauses) {
                if(uniqueClauses.contains(clause))  {
                    for (QueryStats stat : stats) {
                        if(stat.token.equals(clause.getQuery().toString(LuceneConstants.CONTENTS)))
                        {
                            stat.tfq++;
                        }
                    }
                    continue;
                }

                uniqueClauses.add(clause);
                QueryStats newStat = new QueryStats();
                newStat.token = clause.getQuery().toString(LuceneConstants.CONTENTS);
                newStat.tfq = 1;
                newStat.df = 0;

                if(results!=null) {
                    for (DocResults result : results) {
                        if(result.hasTerm(newStat.token)){
                            newStat.df++;
                        }
                    }
                }

                if(newStat.df==0) newStat.idf=0;
                else {
                    double frac =(double)indexReader.numDocs()/(double)newStat.df;
                    newStat.idf = Math.log10(frac);
                    newStat.idf = Math.round(newStat.idf*1000.0)/1000.0;
                }

                stats.add(newStat);
            }
        }
        return stats;
    }*/
}
