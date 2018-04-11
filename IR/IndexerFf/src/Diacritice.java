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

import java.util.ArrayList;
import java.util.Arrays;




public class Diacritice {
  public static ArrayList<String> FindAllPossibleDiacriticWords(String query) {
    char[] charArrayStrippedOfDiacritics = query.toLowerCase().toCharArray();
    for(int i=0; i<query.length();i++)
    {
      switch (charArrayStrippedOfDiacritics[i])
      {
        case 'ă':
        case 'â':
          charArrayStrippedOfDiacritics[i] = 'a';
          break;
        case 537:
        case 351:
          charArrayStrippedOfDiacritics[i] = 's';
          break;
        case 539:
        case 355:
          charArrayStrippedOfDiacritics[i] = 't';
          break;
        case 'î':
          charArrayStrippedOfDiacritics[i] = 'i';
          break;
      }
    }


    ArrayList<String> returnArray = new ArrayList<>();
    ArrayList<Pair> queriesToBeProcessed = new ArrayList<>();
    returnArray.add(new String(charArrayStrippedOfDiacritics));
    queriesToBeProcessed.add(new Pair(charArrayStrippedOfDiacritics,0));

    while (!queriesToBeProcessed.isEmpty())
    {
      Pair queryTested = queriesToBeProcessed.get(0);
      queriesToBeProcessed.remove(0);

      for (int i = queryTested.getIndex(); i < queryTested.getQuery().length; i++)
      {
        char[] queryTestedCopy = Arrays.copyOf(queryTested.getQuery(),queryTested.getQuery().length);
        switch (queryTested.getQuery()[i])
        {
          case 'a':
            queryTestedCopy[i]='ă';
            returnArray.add(new String(queryTestedCopy));
            queriesToBeProcessed.add(new Pair(queryTestedCopy,i+1));
            char[] queryTestedCopyCopy = Arrays.copyOf(queryTestedCopy,queryTestedCopy.length);
            queryTestedCopyCopy[i]='â';
            returnArray.add(new String(queryTestedCopyCopy));
            queriesToBeProcessed.add(new Pair(queryTestedCopyCopy,i+1));
            break;
          case 'i':
            queryTestedCopy[i]='î';
            returnArray.add(new String(queryTestedCopy));
            queriesToBeProcessed.add(new Pair(queryTestedCopy,i+1));
            break;
          case 's':
            queryTestedCopy[i]=537;
            returnArray.add(new String(queryTestedCopy));
            queriesToBeProcessed.add(new Pair(queryTestedCopy,i+1));
            break;
          case 't':
            queryTestedCopy[i]=539;
            returnArray.add(new String(queryTestedCopy));
            queriesToBeProcessed.add(new Pair(queryTestedCopy,i+1));
            break;
        }
      }
    }
    return returnArray;
  }
}
