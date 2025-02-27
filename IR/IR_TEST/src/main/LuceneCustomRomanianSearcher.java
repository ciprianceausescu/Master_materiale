package main;

/**
 * Created by Ciprian Mihai on 3/20/2018.
 */

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.*;

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
//Clasa Searcher, care realizează interogările din aplicație
public class LuceneCustomRomanianSearcher {

    IndexReader indexReaderObject;

    IndexSearcher indexSearcherObject;

    ArrayList<DocResults> results;

    Query query;

    public LuceneCustomRomanianSearcher(String indexDirectoryPath, String query) throws IndexNotFoundException, IOException, InvalidTokenOffsetsException {
        RomanianAnalyzer analyzer = new RomanianAnalyzer();
        Directory directory = FSDirectory.open(Paths.get(indexDirectoryPath));
        results = new ArrayList<>();

        String queryWithoutDiacritics = new String(RomanianSpecialCharacters.stripOfDiacritics(query));
        try {
            this.query = new QueryParser(LuceneConstantsFields.CONTENTS, analyzer).parse(queryWithoutDiacritics);
        }
        catch (ParseException e) {
            e.printStackTrace();
        }

        int hitsPerPage = 100;
        indexReaderObject = DirectoryReader.open(directory);
        indexSearcherObject = new IndexSearcher(indexReaderObject);
        TopDocs docs = indexSearcherObject.search(this.query, hitsPerPage);

        ScoreDoc[] hits = docs.scoreDocs;
        for (ScoreDoc hit : hits) {
            DocResults newDoc = new DocResults();
            String path = indexSearcherObject.doc(hit.doc).get(LuceneConstantsFields.FILE_NAME);
            File filePath;

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
            newDoc.setDocumentUniquiIdentifier(hit.doc);
            newDoc.setDocumentScore(Math.round(hit.score*1000.0)/1000.0);
            newDoc.setDocumentName(indexSearcherObject.doc(hit.doc).get(LuceneConstantsFields.FILE_NAME));
            newDoc.setTermFrequenciesSimpleList(computeTermFreq(hit.doc));
            results.add(newDoc);
        }

        System.out.println("Found results in: " + results.size() + " documents:");
        Collections.sort(results, new DocResultComparator());

        //Pentru highlighter folosim tag-uri html
        SimpleHTMLFormatter simpleHTMLFormatterObject = new SimpleHTMLFormatter();

        QueryScorer scorer = new QueryScorer(this.query);

        UnifiedHighlighter unifiedHighlighterObject = new UnifiedHighlighter(indexSearcherObject, analyzer);

        org.apache.lucene.search.highlight.Highlighter highlighter2 =
                new org.apache.lucene.search.highlight.Highlighter(simpleHTMLFormatterObject, new org.apache.lucene.search.highlight.Scorer() {
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

        //Iterăm prin rezultate
        for (int i = 0; i < hits.length; i++)
        {
            int docid = hits[i].doc;
            Document doc = indexSearcherObject.doc(docid);
            String title = indexSearcherObject.doc(docid).get(LuceneConstantsFields.FILE_NAME);

            //Afișăm rezultatele
            System.out.println("Path" + " : " + title);
        }

        String[] highlights = unifiedHighlighterObject.highlight(LuceneConstantsFields.CONTENTS, this.query,docs,5);

        System.out.println("\nHIGHLIGHTS: ");
        for (String highlight : highlights) {
            System.out.println("\nHIGHLIGHT RESULT:\n" + highlight);
        }
    }

    public List<QueryStatuses> statusQuery(){
        if(query ==null) return null;

        List<QueryStatuses> stats = new ArrayList<>();

        List<BooleanClause> clauses = null;
        if(query instanceof BooleanQuery)
        {
            clauses = ((BooleanQuery) query).clauses();
        }
        //In funcție de interogare poate fi null sau o listă cu 2 sau mai multe cuvinte căutate
        if(clauses == null){
            //Când avem un singur element în interogare
            QueryStatuses newStat = new QueryStatuses();
            newStat.tokenElement = query.toString(LuceneConstantsFields.CONTENTS);
            //Aici trebuie să fie 0 / 1 în cazul în care elementul se găsește sau nu în document
            newStat.tfq = 1;
            newStat.df = 0;
            if(results!=null) {
                for (DocResults result : results) {
                    if(result.hasTerm(newStat.tokenElement)){
                        newStat.df++;
                    }
                }
            }
        if(newStat.idf>0)
            newStat.idf = (float)Math.log(indexReaderObject.numDocs()/newStat.df);
        stats.add(newStat);
        }
        else {
            //Când avem mai multe elemente în interogare
            List<BooleanClause> uniqueClauses = new ArrayList<>();
            for (BooleanClause clause : clauses) {
                if(uniqueClauses.contains(clause))  {
                    for (QueryStatuses stat : stats) {
                        if(stat.tokenElement.equals(clause.getQuery().toString(LuceneConstantsFields.CONTENTS)))
                        {
                            stat.tfq++;
                        }
                    }
                continue;
                }

                uniqueClauses.add(clause);
                QueryStatuses newStat = new QueryStatuses();
                newStat.tokenElement = clause.getQuery().toString(LuceneConstantsFields.CONTENTS);
                newStat.tfq = 1;
                newStat.df = 0;

                if(results!=null) {
                    for (DocResults result : results) {
                        if(result.hasTerm(newStat.tokenElement)){
                            newStat.df++;
                        }
                    }
                }

                if(newStat.df==0)
                    newStat.idf=0;
                else {
                    double frac =(double) indexReaderObject.numDocs()/(double)newStat.df;
                    newStat.idf = Math.log10(frac);
                    newStat.idf = Math.round(newStat.idf*1000.0)/1000.0;
                }

                stats.add(newStat);
            }
        }
        return stats;
    }

    public List<TermFrequencies> computeTermFreq(int docId) throws IOException {

        List<TermFrequencies> termFrequenciesList = new ArrayList<>();

        TermsEnum termsEnum = MultiFields.getTerms(indexReaderObject, LuceneConstantsFields.CONTENTS).iterator();
        PostingsEnum postings = null;

        Terms vector = indexReaderObject.getTermVector(docId, LuceneConstantsFields.CONTENTS);

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
                if(query instanceof BooleanQuery)
                {
                    clauses = ((BooleanQuery) query).clauses();
                }

                if(clauses == null){
                    if (query.toString(LuceneConstantsFields.CONTENTS).equals(term)) {
                        postings = termsEnum.postings(null, PostingsEnum.FREQS);
                        while (postings.nextDoc() != DocIdSetIterator.NO_MORE_DOCS) {
                            tf = postings.freq();
                            termFrequenciesList.add(new TermFrequencies(term, tf));
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
                                for (TermFrequencies termFrequency : termFrequenciesList) {
                                    if(termFrequency.term.equals(term))
                                        contains = true;
                                }

                                if(!contains) {
                                    termFrequenciesList.add(new TermFrequencies(term, tf));
                                }
                            }
                        }
                    }
                }
            }
        }
        return termFrequenciesList;
    }

    Map<String, Float> getTFs(Map<String, Float> docFrequencies) throws IOException {
        TFIDFSimilarity tfidfSim = new ClassicSimilarity();
        Map<String, Float> termFrequencies = new HashMap<>();

        for (int docID = 0; docID < indexReaderObject.maxDoc(); docID++) {
            TermsEnum termsEnum = MultiFields.getTerms(indexReaderObject, LuceneConstantsFields.CONTENTS).iterator();
            PostingsEnum docsEnum = null;

            Terms vector = indexReaderObject.getTermVector(docID, LuceneConstantsFields.CONTENTS);

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

        Fields fieldsObject = MultiFields.getFields(indexReaderObject); //Get the Fields of the index

        Map<String, Float> docFrequenciesHashMap = new HashMap<>();

        TFIDFSimilarity tfidfSIM = new ClassicSimilarity();

        for (String fiel : fieldsObject) {
            TermsEnum termEnum = MultiFields.getTerms(indexReaderObject, fiel).iterator();
            BytesRef bytesRef;
            while ((bytesRef = termEnum.next()) != null) {
                if (termEnum.seekExact(bytesRef)) {
                    String term = bytesRef.utf8ToString();
                    float idf = termEnum.docFreq();
                    docFrequenciesHashMap.put(term, idf);
                }
            }
        }
        return docFrequenciesHashMap;
    }

    public ArrayList<DocResults> getResultedDocuments() {
    return results;
  }

    public ArrayList<Document> getDocuments(ArrayList<Integer> docIds) throws IOException {
        ArrayList<Document> resultedDocuments = new ArrayList<>();
        for (Integer docId : docIds) {
            resultedDocuments.add(indexSearcherObject.doc(docId));
        }
        return resultedDocuments;
    }

    public void close() throws IOException {
        indexReaderObject.close();
    }

    public void Backup() throws IOException {
        long sumTotalTermFreq = indexReaderObject.getSumTotalTermFreq(LuceneConstantsFields.CONTENTS);
        long sumtDocFreq = indexReaderObject.getSumDocFreq(LuceneConstantsFields.CONTENTS);
        Terms termVector = indexReaderObject.getTermVector(0, LuceneConstantsFields.CONTENTS);
        Terms[] docFreqs = new Terms[indexReaderObject.numDocs()];

        for (int i = 0; i < indexReaderObject.numDocs(); i++) {
            docFreqs[i] = indexReaderObject.getTermVector(i, LuceneConstantsFields.CONTENTS);
        }
        Bits liveDocs = MultiFields.getLiveDocs(indexReaderObject);
        TermsEnum termEnum = MultiFields.getTerms(indexReaderObject, LuceneConstantsFields.CONTENTS).iterator();
        BytesRef term = null;
        int docCount = indexReaderObject.numDocs();

        while ((term = termEnum.next()) != null) {
            List<LeafReaderContext> leafs = indexReaderObject.leaves();
        }
    }
}
