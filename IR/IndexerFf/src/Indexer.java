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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Paths;

import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.poifs.filesystem.FileMagic;
import org.apache.poi.xwpf.extractor.XWPFWordExtractor;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.tika.Tika;
import org.jsoup.Jsoup;

public class Indexer {

  private IndexWriter writer;

  public Indexer(String indexDirectoryPath) throws IOException {

    Directory indexDirectory =
        FSDirectory.open(Paths.get(indexDirectoryPath));

    IndexWriterConfig config = new IndexWriterConfig(new RomanianAnalyzer());
    //config.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);

    writer = new IndexWriter(indexDirectory, config);
  }

  public void close() throws IOException {
    writer.close();
  }

  private Document getDocument(File file) throws IOException {
    Document document = new Document();
    Field contents = null;
    String type = new Tika().detect(file);

    if(type.contains("html")) {

      contents = new Field(LuceneConstants.CONTENTS,
          Jsoup.parse(file,null,"127.0.0.1").text(),
          TextField.TYPE_STORED);

    }else if(type.contains("pdf")) {

      contents = new Field(LuceneConstants.CONTENTS,
          new PDFTextStripper().getText(PDDocument.load(file)),
          TextField.TYPE_STORED);

    }else if(type.contains("doc")){

      BufferedInputStream wordInputStream = new BufferedInputStream(new FileInputStream(file.getCanonicalPath()));
      String text = null;
      if (FileMagic.valueOf(wordInputStream) == FileMagic.OLE2) {
        WordExtractor ex = new WordExtractor(wordInputStream);
        text = ex.getText();
        ex.close();
      } else if(FileMagic.valueOf(wordInputStream) == FileMagic.OOXML) {
        XWPFDocument doc = new XWPFDocument(wordInputStream);
        XWPFWordExtractor extractor = new XWPFWordExtractor(doc);
        text = extractor.getText();
        extractor.close();
      }
      contents = new Field(LuceneConstants.CONTENTS, text,
          TextField.TYPE_STORED);

    }else if(type.contains("plain")) {

       contents = new Field(LuceneConstants.CONTENTS,
          new java.util.Scanner(file,"UTF-8").useDelimiter("\\A").next(),
          TextField.TYPE_STORED);
    }
    Field fileNameField = new Field(LuceneConstants.FILE_NAME,
        file.getName(),StoredField.TYPE);

    Field filePathField = new Field(LuceneConstants.FILE_PATH,
        file.getCanonicalPath(),StoredField.TYPE);

    if(contents!=null) {
      document.add(contents);
      document.add(fileNameField);
      document.add(filePathField);
    }
    return document;
  }

  private void indexFile(File file) throws IOException {
    System.out.println("Indexing "+file.getCanonicalPath());
    String type = new Tika().detect(file);
    System.out.println("Type: "+ type);
    Document document = getDocument(file);
    writer.addDocument(document);
  }

  public int createIndex(String dataDirPath, LuceneFileFilter filter)
      throws IOException {

    File[] files = new File(dataDirPath).listFiles();
    int innerDocs=0;

    for (File file : files) {
      if(!file.isDirectory() && !file.isHidden()
          && file.exists() && file.canRead() && filter.accept(file)){
        indexFile(file);
      }else if(file.isDirectory() && file.exists() && file.canRead()){
        innerDocs+=createIndex(file.getCanonicalPath(),filter);
      }
    }
    return innerDocs+writer.numDocs();
  }

}