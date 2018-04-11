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

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.tartarus.snowball.ext.RomanianStemmer;

public class Searcher {

  IndexReader indexReader;

  IndexSearcher indexSearcher;

  ArrayList<Document> resultedDocuments;

  public Searcher(String indexDirectoryPath, String query) throws IndexNotFoundException, IOException{
    RomanianAnalyzer analyzer = new RomanianAnalyzer();
    ArrayList<Integer> resultedDocIds = new ArrayList<>();

    Directory directory = FSDirectory.open(Paths.get(indexDirectoryPath));

    ArrayList<String> allQueries = Diacritice.FindAllPossibleDiacriticWords(query);
    Query[] q = new Query[allQueries.size()];
    try {
      for(int i=0;i<allQueries.size();i++)
        q[i] = new QueryParser(LuceneConstants.CONTENTS, analyzer).parse(allQueries.get(i));
    } catch (ParseException e) {
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
          newDoc.setScore(hit.score);
          results.add(newDoc);
          resultedDocIds.add(hit.doc);
        }
      }
    }


    System.out.println("Found " + resultedDocIds.size() + " hits.");
    resultedDocuments = getDocuments(resultedDocIds);

    int i=0;
    for (Document doc : resultedDocuments) {
      System.out.println(doc.get(LuceneConstants.FILE_NAME) +
          "\t content: " + doc.get(LuceneConstants.CONTENTS));
      results.get(i).setDocName(doc.get(LuceneConstants.FILE_NAME));
    }

  }

  public ArrayList<Document> getResultedDocuments() {
    return resultedDocuments;
  }

  public ArrayList<Document> getDocuments(ArrayList<Integer> docIds) throws IOException {
    ArrayList<Document> resultedDocuments = new ArrayList<>();
    for(Integer docId: docIds){
      resultedDocuments.add(indexSearcher.doc(docId));
    }
    return resultedDocuments;
  }

  public void close() throws IOException {
    indexReader.close();
  }
}