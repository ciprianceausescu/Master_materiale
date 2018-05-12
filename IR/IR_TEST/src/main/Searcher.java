/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package main;

import javax.swing.text.Highlighter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Formatter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.googlecode.mp4parser.authoring.Track;
import com.googlecode.mp4parser.authoring.builder.Fragmenter;
import com.sun.org.apache.xpath.internal.operations.Bool;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.Fields;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexOptions;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.LeafReaderContext;
import org.apache.lucene.index.MultiFields;
import org.apache.lucene.index.MultiPostingsEnum;
import org.apache.lucene.index.PostingsEnum;
import org.apache.lucene.index.PrefixCodedTerms;
import org.apache.lucene.index.Term;
import org.apache.lucene.index.TermState;
import org.apache.lucene.index.Terms;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.DocIdSetIterator;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Scorer;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.Weight;
import org.apache.lucene.search.highlight.*;
import org.apache.lucene.search.similarities.BM25Similarity;
import org.apache.lucene.search.similarities.ClassicSimilarity;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Bits;
import org.apache.lucene.util.BytesRef;
import org.tartarus.snowball.ext.RomanianStemmer;

public class Searcher {

  IndexReader indexReader;

  IndexSearcher indexSearcher;

  ArrayList<DocResults> results;

  Query q;

  public Searcher(String indexDirectoryPath, String query) throws IndexNotFoundException, IOException, InvalidTokenOffsetsException {
    RomanianAnalyzer analyzer = new RomanianAnalyzer();
    Directory directory = FSDirectory.open(Paths.get(indexDirectoryPath));
    results = new ArrayList<>();

    String queryWithoutDiacritics = new String(Diacritice.stripOfDiacritics(query));
    try {
      q= new QueryParser(LuceneConstants.CONTENTS, analyzer).parse(queryWithoutDiacritics);
    } catch (ParseException e) {
      e.printStackTrace();
    }

    int hitsPerPage = 100;
    indexReader = DirectoryReader.open(directory);
    indexSearcher = new IndexSearcher(indexReader);
    TopDocs docs = indexSearcher.search(q, hitsPerPage);

    ScoreDoc[] hits = docs.scoreDocs;
    for (ScoreDoc hit : hits) {
      DocResults newDoc = new DocResults();
      newDoc.setDocID(hit.doc);
      newDoc.setScore(Math.round(hit.score*1000.0)/1000.0);
      newDoc.setDocName(indexSearcher.doc(hit.doc).get(LuceneConstants.FILE_NAME));
      newDoc.setTermsFreq(computeTermFreq(hit.doc));
      results.add(newDoc);
    }

    System.out.println("Found " + results.size() + " hits.");

    results.sort(new Comparator<DocResults>() {
      @Override
      public int compare(DocResults o1, DocResults o2) {
        int ret = (int) (o1.getScore() - o2.getScore());
        return ret;
      }
    });

    //Uses HTML &lt;B&gt;&lt;/B&gt; tag to highlight the searched terms
    SimpleHTMLFormatter formatter = new SimpleHTMLFormatter();

    //It scores text fragments by the number of unique query terms found
    //Basically the matching score in layman terms
    QueryScorer scorer = new QueryScorer(q);


    org.apache.lucene.search.highlight.Fragmenter fragmenter =  new SimpleFragmenter();

    //It breaks text up into same-size texts but does not split up spans
    //SimpleSpanFragmenter fragmenter = new SimpleSpanFragmenter(scorer, 10);

    //breaks text up into same-size fragments with no concerns over spotting sentence boundaries.
    //Fragmenter fragmenter = new SimpleFragmenter(10);

    //set fragmenter to highlighter
    //highlighter.setTextFragmenter(fragmenter);

    //Iterate over found results
    for (int i = 0; i < hits.length; i++)
    {
      int docid = hits[i].doc;
      Document doc = indexSearcher.doc(docid);
      String title = doc.get("path");

      //Printing - to which document result belongs
      System.out.println("Path " + " : " + title);

      //Get stored text from found document
      String text = doc.get("contents");

      //Create token stream
      //TokenStream stream = TokenSources.getAnyTokenStream(reader, docid, "contents", analyzer);

      //Get highlighted text fragments
      //String[] frags = highlighter.getBestFragments(stream, text, 10);

    }
  }

  public List<QueryStats> statusQuery(){
    if(q==null) return null;

    List<QueryStats> stats = new ArrayList<>();

    List<BooleanClause> clauses = null;
    if(q instanceof BooleanQuery)
    {
      clauses = ((BooleanQuery) q).clauses();
    }

    if(clauses == null){
      QueryStats newStat = new QueryStats();
      newStat.token = q.toString(LuceneConstants.CONTENTS);
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

  public List<TermFrequencies> computeTermFreq(int docId) throws IOException {

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

  Map<String, Float> getTFs(Map<String, Float> docFrequencies) throws IOException {
    TFIDFSimilarity tfidfSim = new ClassicSimilarity();
    Map<String, Float> termFrequencies = new HashMap<>();

    for (int docID = 0; docID < indexReader.maxDoc(); docID++) {
      TermsEnum termsEnum = MultiFields.getTerms(indexReader, LuceneConstants.CONTENTS).iterator();
      PostingsEnum docsEnum = null;

      Terms vector = indexReader.getTermVector(docID, LuceneConstants.CONTENTS);

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

          docsEnum = termsEnum.postings(null, PostingsEnum.FREQS);
          while (docsEnum.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
            tf = docsEnum.freq();
            termFrequencies.put(term, tf);
          }

          float idf = docFrequencies.get(term);
          float w = tf * idf;
          // tf_Idf_Weights.put(term, w);
        }
      }
    }
    return termFrequencies;
  }

  Map<String, Float> getIdfs(String field) throws IOException {
    /** GET FIELDS **/
    Fields fields = MultiFields.getFields(indexReader); //Get the Fields of the index

    Map<String, Float> docFrequencies = new HashMap<>();

    TFIDFSimilarity tfidfSIM = new ClassicSimilarity();

    for (String fiel : fields) {
      TermsEnum termEnum = MultiFields.getTerms(indexReader, fiel).iterator();
      BytesRef bytesRef;
      while ((bytesRef = termEnum.next()) != null) {
        if (termEnum.seekExact(bytesRef)) {
          String term = bytesRef.utf8ToString();
          // float idf = tfidfSIM.idf(termEnum.docFreq(), indexReader.numDocs());
          float idf = termEnum.docFreq();
          docFrequencies.put(term, idf);
        }
      }
    }

    return docFrequencies;
  }

  public ArrayList<DocResults> getResultedDocuments() {
    return results;
  }

  public ArrayList<Document> getDocuments(ArrayList<Integer> docIds) throws IOException {
    ArrayList<Document> resultedDocuments = new ArrayList<>();
    for (Integer docId : docIds) {
      resultedDocuments.add(indexSearcher.doc(docId));
    }
    return resultedDocuments;
  }

  public void close() throws IOException {
    indexReader.close();
  }

  public void Backup() throws IOException {
    //int docFreq = indexReader.docFreq(new Term(LuceneConstants.CONTENTS, q[0].toString(LuceneConstants.CONTENTS)));
    long sumTotalTermFreq = indexReader.getSumTotalTermFreq(LuceneConstants.CONTENTS);
    long sumtDocFreq = indexReader.getSumDocFreq(LuceneConstants.CONTENTS);
    Terms termVector = indexReader.getTermVector(0, LuceneConstants.CONTENTS);
    Terms[] docFreqs = new Terms[indexReader.numDocs()];

    for (int i = 0; i < indexReader.numDocs(); i++) {
      docFreqs[i] = indexReader.getTermVector(i, LuceneConstants.CONTENTS);
    }
    //long totalTermFreq = indexReader.totalTermFreq(new Term(LuceneConstants.CONTENTS, q[0].toString(LuceneConstants.CONTENTS)));

    Bits liveDocs = MultiFields.getLiveDocs(indexReader);
    TermsEnum termEnum = MultiFields.getTerms(indexReader, LuceneConstants.CONTENTS).iterator();
    BytesRef term = null;
    int docCount = indexReader.numDocs();

    while ((term = termEnum.next()) != null) {
      //Term termQuerried = new Term(LuceneConstants.CONTENTS, q[0].toString(LuceneConstants.CONTENTS));
      //long indexDf = indexReader.docFreq(termQuerried);
      // if (termQuerried.bytes().equals(term)) {
      //indexSearcher.search(q[0], hitsPerPage);
      //TermQuery termQuery = new TermQuery(termQuerried);
      //Weight weight = termQuery.createWeight(indexSearcher, ScoreMode.COMPLETE, 0);
      List<LeafReaderContext> leafs = indexReader.leaves();
    }
    //getTFs(getIdfs(LuceneConstants.CONTENTS));
  }

}
