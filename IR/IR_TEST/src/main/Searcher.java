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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.*;

import com.sun.deploy.util.StringUtils;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.Fields;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.LeafReaderContext;
import org.apache.lucene.index.MultiFields;
import org.apache.lucene.index.PostingsEnum;
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
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.highlight.*;
import org.apache.lucene.search.similarities.ClassicSimilarity;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.search.uhighlight.UnifiedHighlighter;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Bits;
import org.apache.lucene.util.BytesRef;

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
            q= new QueryParser(LuceneConstantsFields.CONTENTS, analyzer).parse(queryWithoutDiacritics);
        }
        catch (ParseException e) {
            e.printStackTrace();
        }

        int hitsPerPage = 100;
        indexReader = DirectoryReader.open(directory);
        indexSearcher = new IndexSearcher(indexReader);
        TopDocs docs = indexSearcher.search(q, hitsPerPage);

        ScoreDoc[] hits = docs.scoreDocs;
        for (ScoreDoc hit : hits) {
            DocResults newDoc = new DocResults();
            String path = indexSearcher.doc(hit.doc).get(LuceneConstantsFields.FILE_NAME);
            File filePath;
            //Eroare daca nu indexezi fisierul pentru ca file
            if(LuceneConstantsFields.FILES_PATH != null)
                filePath = new File(LuceneConstantsFields.FILES_PATH + "\\" + path);
            else
                filePath = new File("C:\\Users\\Ciprian Mihai\\Documents\\Docs\\" + path);

            int count;
            try(Scanner sc = new Scanner(new FileInputStream(filePath))){
                count=0;
                while(sc.hasNext()){
                    sc.next();
                    count++;
                }
            }

            newDoc.setOccurenceOfWord(count);
            newDoc.setDocUniquiIdentifier(hit.doc);
            newDoc.setDocScore(Math.round(hit.score*1000.0)/1000.0);
            newDoc.setDocumentName(indexSearcher.doc(hit.doc).get(LuceneConstantsFields.FILE_NAME));
            newDoc.setTermFrequenciesList(computeTermFreq(hit.doc));
            results.add(newDoc);
        }

        System.out.println("Found results in: " + results.size() + " documents:");
        Collections.sort(results, new DocResultComparator());

        //Uses HTML &lt;B&gt;&lt;/B&gt; tag to highlight the searched terms
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter();

        //It scores text fragments by the number of unique query terms found
        //Basically the matching score in layman terms
        QueryScorer scorer = new QueryScorer(q);

        //used to markup highlighted terms found in the best sections of a text
        UnifiedHighlighter highlighter = new UnifiedHighlighter(indexSearcher, analyzer);

        org.apache.lucene.search.highlight.Highlighter highlighter2 =
                new org.apache.lucene.search.highlight.Highlighter(formatter, new org.apache.lucene.search.highlight.Scorer() {
                    @Override
                    public TokenStream init(TokenStream tokenStream) throws IOException {
                        return null;
                    }

                    @Override
                    public void startFragment(TextFragment newFragment) {

                    }

                    @Override
                    public float getTokenScore() {
                        return 0;
                    }

                    @Override
                    public float getFragmentScore() {
                        return 0;
                    }
                });

        org.apache.lucene.search.highlight.Fragmenter fragmenter =  new SimpleFragmenter();
        highlighter2.setTextFragmenter(fragmenter);
        String[] fragments = highlighter2.getBestFragments(analyzer,LuceneConstantsFields.CONTENTS,"iez",5);
        for (String fragment : fragments) {
            System.out.println("\n\n FRAGMENTS : "+fragment);
        }

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
            String title = indexSearcher.doc(docid).get(LuceneConstantsFields.FILE_NAME);

            //Printing - to which document result belongs
            System.out.println("Path" + " : " + title);

            //Get stored text from found document
            String text = doc.get("contents");
        }

        String[] highlights = highlighter.highlight(LuceneConstantsFields.CONTENTS,q,docs,5);

        System.out.println("\nHIGHLIGHTS: ");
        for (String highlight : highlights) {
            System.out.println("\nHIGHLIGHT RESULT:\n" + highlight);
        }

        /*ArrayList<String> filteredFragments = new ArrayList<>();

        HashSet<String> alreadyShown = new HashSet<>();
        for (String highlight : highlights) {
            int indexOfTerm = highlight.indexOf("<b>") + 3;
            String term = highlight.substring(indexOfTerm, indexOfTerm + 3);
            term = term.toLowerCase();

            if (alreadyShown.contains(term))
                continue;
            alreadyShown.add(term);

            filteredFragments.add(highlight);
        }
        System.out.println();
        System.out.println("HIGHLIGHTS: ");
        System.out.println(StringUtils.join(filteredFragments, "..."));*/
    }

    public List<QueryStats> statusQuery(){
        if(q==null) return null;

        List<QueryStats> stats = new ArrayList<>();

        List<BooleanClause> clauses = null;
        if(q instanceof BooleanQuery)
        {
            clauses = ((BooleanQuery) q).clauses();
        }
        //In functie de interogare poate fi null sau o lista cu 2 sau mai multe cuvinte cautare
        if(clauses == null){
            //Cand avem un singur element in interogare
            QueryStats newStat = new QueryStats();
            newStat.token = q.toString(LuceneConstantsFields.CONTENTS);
            //Cred ca aici trebuie sa fie 0 / 1 in cazul in care elementul se gaseste sau nu in document
            newStat.tfq = 1;
            newStat.df = 0;
            if(results!=null) {
                for (DocResults result : results) {
                    if(result.hasTerm(newStat.token)){
                        newStat.df++;
                    }
                }
            }
        if(newStat.idf>0)
            newStat.idf = (float)Math.log(indexReader.numDocs()/newStat.df);
        stats.add(newStat);
        }
        else {
            //Cand avem mai multe elemente in interogare
            List<BooleanClause> uniqueClauses = new ArrayList<>();
            for (BooleanClause clause : clauses) {
                if(uniqueClauses.contains(clause))  {
                    for (QueryStats stat : stats) {
                        if(stat.token.equals(clause.getQuery().toString(LuceneConstantsFields.CONTENTS)))
                        {
                            stat.tfq++;
                        }
                    }
                continue;
                }

                uniqueClauses.add(clause);
                QueryStats newStat = new QueryStats();
                newStat.token = clause.getQuery().toString(LuceneConstantsFields.CONTENTS);
                newStat.tfq = 1;
                newStat.df = 0;

                if(results!=null) {
                    for (DocResults result : results) {
                        if(result.hasTerm(newStat.token)){
                            newStat.df++;
                        }
                    }
                }

                if(newStat.df==0)
                    newStat.idf=0;
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

        TermsEnum termsEnum = MultiFields.getTerms(indexReader, LuceneConstantsFields.CONTENTS).iterator();
        PostingsEnum postings = null;

        Terms vector = indexReader.getTermVector(docId, LuceneConstantsFields.CONTENTS);

        try {
            termsEnum = vector.iterator();
        }
        catch (NullPointerException e) {
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
                    if (q.toString(LuceneConstantsFields.CONTENTS).equals(term)) {
                        postings = termsEnum.postings(null, PostingsEnum.FREQS);
                        while (postings.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
                            tf = postings.freq();
                            termFrequencies.add(new TermFrequencies(term, tf));
                        }
                    }
                }
                else {
                    for (BooleanClause clause : clauses) {
                        if (clause.getQuery().toString(LuceneConstantsFields.CONTENTS).equals(term)) {
                            postings = termsEnum.postings(null, PostingsEnum.FREQS);
                            while (postings.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
                                tf = postings.freq();
                                boolean contains=false;
                                for (TermFrequencies termFrequency : termFrequencies) {
                                    if(termFrequency.term.equals(term))
                                        contains = true;
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
            TermsEnum termsEnum = MultiFields.getTerms(indexReader, LuceneConstantsFields.CONTENTS).iterator();
            PostingsEnum docsEnum = null;

            Terms vector = indexReader.getTermVector(docID, LuceneConstantsFields.CONTENTS);

            try {
                termsEnum = vector.iterator();
            }
            catch (NullPointerException e) {
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
        long sumTotalTermFreq = indexReader.getSumTotalTermFreq(LuceneConstantsFields.CONTENTS);
        long sumtDocFreq = indexReader.getSumDocFreq(LuceneConstantsFields.CONTENTS);
        Terms termVector = indexReader.getTermVector(0, LuceneConstantsFields.CONTENTS);
        Terms[] docFreqs = new Terms[indexReader.numDocs()];

        for (int i = 0; i < indexReader.numDocs(); i++) {
            docFreqs[i] = indexReader.getTermVector(i, LuceneConstantsFields.CONTENTS);
        }
        Bits liveDocs = MultiFields.getLiveDocs(indexReader);
        TermsEnum termEnum = MultiFields.getTerms(indexReader, LuceneConstantsFields.CONTENTS).iterator();
        BytesRef term = null;
        int docCount = indexReader.numDocs();

        while ((term = termEnum.next()) != null) {
            List<LeafReaderContext> leafs = indexReader.leaves();
        }
    }
}
