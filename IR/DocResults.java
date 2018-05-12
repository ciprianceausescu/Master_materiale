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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;

public class DocResults {
  private double score;
  private int docID;
  private String docName;
  private List<TermFrequencies> termsFreq;

  public String getDocName() {
    return docName;
  }

  public void setDocName(String docName) {
    this.docName = docName;
  }

  public double getScore() {

    return score;
  }

  @Override
  public String toString() {
    String docResultsString = docName + "\t score=" + score + "\t ";
    for (TermFrequencies termFrequencies : termsFreq) {
      docResultsString += termFrequencies.toString();
    }
  return docResultsString;
  }

  public void setScore(double score) {
    this.score = score;
  }

  public int getDocID() {   return docID;  }

  public void setDocID(int docID) {
    this.docID = docID;
  }

  public List<TermFrequencies> getTermsFreq() {
    return termsFreq;
  }

  public void setTermsFreq(List<TermFrequencies> newtermsFreq) {
    this.termsFreq = newtermsFreq;
  }

  boolean hasTerm(String term){
    for (TermFrequencies termFrequencies : termsFreq) {
      if(termFrequencies.term.equals(term)) return true;
    }
    return false;
  }
}
