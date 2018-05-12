package main;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.*;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;
import org.tartarus.snowball.ext.RomanianStemmer;

public class Searcher {

    IndexReader indexReader;

    IndexSearcher indexSearcher;

    ArrayList<Document> resultedDocuments;

    List<QueryStats> queryStats;

    public Searcher(String indexDirectoryPath, String query) throws IndexNotFoundException, IOException, ParseException {
        RoAnalyzer analyzer = new RoAnalyzer();
        ArrayList<Integer> resultedDocIds = new ArrayList<>();

        Directory directory = FSDirectory.open(Paths.get(indexDirectoryPath));

        ArrayList<String> allQueries = Diacritice.FindAllPossibleDiacriticWords(query);
        Query[] q = new Query[allQueries.size()];
        try {
            for(int i=0;i<allQueries.size();i++)
                q[i] = new QueryParser(LuceneConstants.CONTENTS, analyzer).parse(allQueries.get(i));
        }
        catch (ParseException e) {
            e.printStackTrace();
        }

        int hitsPerPage = 10;
        indexReader = DirectoryReader.open(directory);
        indexSearcher = new IndexSearcher(indexReader);
        TopDocs[] docs =  new TopDocs[allQueries.size()];

        ArrayList<DocResults> results = new ArrayList<>();

        for(int i=0;i<allQueries.size();i++)
        {
            docs[i] = indexSearcher.search(q[i], hitsPerPage);
            ScoreDoc[] hits = docs[i].scoreDocs;
            for (ScoreDoc hit : hits) {
                if(!resultedDocIds.contains(hit.doc))
                {
                    DocResults newDoc = new DocResults();
                    newDoc.setDocID(hit.doc);
                    newDoc.setScore(Math.round(hit.score*1000.0)/1000.0);
                    newDoc.setDocName(indexSearcher.doc(hit.doc).get(LuceneConstants.FILE_NAME));
                    newDoc.setTermsFreq(computeTermFreq(q[i], hit.doc));
                    results.add(newDoc);
                    resultedDocIds.add(hit.doc);
                }
            }
         }


      System.out.println("Found " + resultedDocIds.size() + " hits.");
      resultedDocuments = getDocuments(resultedDocIds);

      int i=0;
      for (Document doc : resultedDocuments) {
          System.out.println(doc.get("path") +
            "\t content: " + doc.get(LuceneConstants.CONTENTS));
          results.get(i).setDocName(doc.get("path"));
      }
      QueryParser parser = new QueryParser("contents", analyzer);
      Query queryObj = parser.parse(query);
      queryStats = statusQuery(queryObj, results, indexReader);
  }

  public ArrayList<Document> getResultedDocuments() {
    return resultedDocuments;
  }

  public ArrayList<Document> getDocuments(ArrayList<Integer> docIds) throws IOException {
      ArrayList<Document> resultedDocuments = new ArrayList<>();
      for(Integer docId: docIds) {
        resultedDocuments.add(indexSearcher.doc(docId));
      }
      return resultedDocuments;
  }

  public void close() throws IOException {
      indexReader.close();
  }

  public static void main(String[] args) throws IOException, ParseException {
      Scanner sc = new Scanner(System.in);
      String query;
      query = sc.nextLine();
      Searcher s = new Searcher("index", query);

      for(QueryStats qs: s.queryStats)
          System.out.println(qs);
  }
  public static List<QueryStats> statusQuery(Query query, ArrayList<DocResults> results, IndexReader indexReader){
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
  }
    public List<TermFrequencies> computeTermFreq(Query q, int docId) throws IOException {

        //TFIDFSimilarity tfidfSim = new ClassicSimilarity();
        List<TermFrequencies> termFrequencies = new ArrayList<>();

        TermsEnum termsEnum = MultiFields.getTerms(indexReader, LuceneConstants.CONTENTS).iterator();
        PostingsEnum postings = null;

        Terms vector = indexReader.getTermVector(docId, LuceneConstants.CONTENTS);

        try {
            termsEnum = vector.iterator();
        } catch (NullPointerException e) {
            e.printStackTrace();
        }

        BytesRef bytesRef = null;
        while ((bytesRef = termsEnum.next()) != null) {
            if (termsEnum.seekExact(bytesRef)) {
                String term = bytesRef.utf8ToString();
                float tf = 0;

                List<BooleanClause> clauses = null;
                if(q instanceof BooleanQuery)
                {
                    clauses = ((BooleanQuery) q).clauses();
                }

                if(clauses == null){
                    if (q.toString(LuceneConstants.CONTENTS).equals(term)) {
                        postings = termsEnum.postings(null, PostingsEnum.FREQS);
                        while (postings.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
                            tf = postings.freq();
                            termFrequencies.add(new TermFrequencies(term, tf));
                        }
                    }
                }else {
                    for (BooleanClause clause : clauses) {
                        if (clause.getQuery().toString(LuceneConstants.CONTENTS).equals(term)) {
                            postings = termsEnum.postings(null, PostingsEnum.FREQS);
                            while (postings.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
                                tf = postings.freq();

                                boolean contains=false;
                                for (TermFrequencies termFrequency : termFrequencies) {
                                    if(termFrequency.term.equals(term)) contains = true;
                                }

                                if(!contains) {
                                    termFrequencies.add(new TermFrequencies(term, tf));
                                }
                            }
                        }
                    }
                }
            }
        }
        return termFrequencies;
    }
}