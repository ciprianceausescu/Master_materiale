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
import java.util.List;

public class DocResults {
    private double docScore;
    private int docUniquiIdentifier;
    private String documentName;
    private List<TermFrequencies> termFrequenciesList;

    public String getDocumentName() {
    return documentName;
  }

    public void setDocumentName(String documentName) {
    this.documentName = documentName;
  }

    public double getDocScore() { return this.docScore; }

    @Override
    public String toString() {
        String docResultsString = documentName + "\t docScore=" + docScore + "\t ";
        for (TermFrequencies termFrequencies : termFrequenciesList) {
          docResultsString += termFrequencies.toString();
        }
        return docResultsString;
    }

    public void setDocScore(double docScore) {
    this.docScore = docScore;
  }

    public int getDocUniquiIdentifier() { return docUniquiIdentifier; }

    public void setDocUniquiIdentifier(int docUniquiIdentifier) {
    this.docUniquiIdentifier = docUniquiIdentifier;
  }

    public List<TermFrequencies> getTermFrequenciesList() {
    return termFrequenciesList;
  }

    public void setTermFrequenciesList(List<TermFrequencies> newtermsFreq) {
    this.termFrequenciesList = newtermsFreq;
  }

    boolean hasTerm(String term){
        for (TermFrequencies termFrequencies : termFrequenciesList) {
            if(termFrequencies.term.equals(term)) return true;
        }
        return false;
    }
}