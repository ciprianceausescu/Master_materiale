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
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;

public class GenerateStopwords {
  final static String[] stopString = {"acea",
      "aceasta",
      "aceea",
      "acei",
      "aceia",
      "acel",
      "acela",
      "acele",
      "acelea",
      "acest",
      "acesta",
      "aceste",
      "acestea",
      "aceşti",
      "aceştia",
      "acolo",
      "acum",
      "ai",
      "aia",
      "aibă",
      "aici",
      "al",
      "ăla",
      "ale",
      "alea",
      "altceva",
      "altcineva",
      "am",
      "ar",
      "are",
      "aş",
      "aşadar",
      "asemenea",
      "asta",
      "astăzi",
      "astea",
      "ăştia",
      "asupra",
      "aţi",
      "au",
      "avea",
      "avem",
      "aveţi",
      "azi",
      "bine",
      "bucur",
      "bună",
      "ca",
      "că",
      "căci",
      "când",
      "care",
      "cărei",
      "căror",
      "cărui",
      "cât",
      "câte",
      "câţi",
      "către",
      "câtva",
      "ce",
      "cel",
      "ceva",
      "chiar",
      "cind",
      "cand",
      "cine",
      "cineva",
      "cat",
      "cate",
      "cati",
      "catva",
      "contra",
      "cu",
      "cum",
      "cumva",
      "curând",
      "da",
      "dă",
      "dacă",
      "dar",
      "datorită",
      "de",
      "deci",
      "deja",
      "deoarece",
      "departe",
      "deşi",
      "din",
      "dinaintea",
      "dintr",
      "dintre",
      "drept",
      "după",
      "ea",
      "ei",
      "el",
      "ele",
      "eram",
      "este",
      "eşti",
      "eu",
      "face",
      "fara",
      "fi",
      "fie",
      "fiecare",
      "fii",
      "fim",
      "fiţi",
      "iar",
      "ieri",
      "îi",
      "îl",
      "îmi",
      "împotriva",
      "în ",
      "înainte",
      "înaintea",
      "încât",
      "încotro",
      "între",
      "întrucât",
      "îţi",
      "la",
      "lângă",
      "le",
      "li",
      "lor",
      "lui",
      "mă",
      "mâine",
      "mea",
      "mei",
      "mele",
      "mereu",
      "meu",
      "mi",
      "mine",
      "mult",
      "multă",
      "mulţi",
      "ne",
      "nicăieri",
      "nici",
      "nimeni",
      "nişte",
      "noastră",
      "noastre",
      "noi",
      "noştri",
      "nostru",
      "nu",
      "ori",
      "oricând",
      "oricare",
      "oricât",
      "orice",
      "oricine",
      "oricat",
      "oricum",
      "oriunde",
      "până",
      "pe",
      "pentru",
      "peste",
      "pana",
      "poate",
      "pot",
      "prea",
      "prima",
      "primul",
      "prin",
      "printr",
      "sa",
      "săi",
      "sale",
      "sau",
      "se",
      "şi",
      "sînt",
      "sîntem",
      "sînteţi",
      "spre",
      "sub",
      "sunt",
      "suntem",
      "sunteţi",
      "ta",
      "tăi",
      "tale",
      "tău",
      "te",
      "ţi",
      "ţie",
      "tine",
      "toată",
      "toate",
      "tot",
      "toţi",
      "totuşi",
      "tu",
      "un",
      "una",
      "unde",
      "undeva",
      "unei",
      "unele",
      "uneori",
      "unor",
      "vă",
      "vi",
      "voastră",
      "voastre",
      "voi",
      "voştri",
      "vostru",
      "vouă",
      "vreo",
      "vreun"};
  public static void main(String[] args) throws IOException {
    PrintWriter writer = new PrintWriter("stopwordsGenerated.txt", "UTF-8");
    for (String s : stopString) {
      char[] charArrayStrippedOfDiacritics = s.toLowerCase().toCharArray();
      for(int i=0; i<s.length();i++)
      {
        switch (charArrayStrippedOfDiacritics[i])
        {
          case 259://ă
          case 226://â
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


      ArrayList<Pair> stringsToBeProcessed = new ArrayList<>();
      stringsToBeProcessed.add(new Pair(charArrayStrippedOfDiacritics,0));

      writer.println(charArrayStrippedOfDiacritics);

      while (!stringsToBeProcessed.isEmpty())
      {
        Pair stringTested = stringsToBeProcessed.get(0);
        stringsToBeProcessed.remove(0);

        for (int i = stringTested.getIndex(); i < stringTested.getQuery().length; i++)
        {
          char[] queryTestedCopy = Arrays.copyOf(stringTested.getQuery(),stringTested.getQuery().length);
          switch (stringTested.getQuery()[i])
          {
            case 'a':
              queryTestedCopy[i]=259;
              writer.println(queryTestedCopy);
              stringsToBeProcessed.add(new Pair(queryTestedCopy,i+1));
              char[] queryTestedCopyCopy = Arrays.copyOf(queryTestedCopy,queryTestedCopy.length);
              queryTestedCopyCopy[i]=226;
              writer.println(queryTestedCopyCopy);
              stringsToBeProcessed.add(new Pair(queryTestedCopyCopy,i+1));
              break;
            case 'i':
              queryTestedCopy[i]='î';
              writer.println(queryTestedCopy);
              stringsToBeProcessed.add(new Pair(queryTestedCopy,i+1));
              break;
            case 's':
              queryTestedCopy[i]=537;
              writer.println(queryTestedCopy);
              stringsToBeProcessed.add(new Pair(queryTestedCopy,i+1));
              break;
            case 't':
              queryTestedCopy[i]=539;
              writer.println(queryTestedCopy);
              stringsToBeProcessed.add(new Pair(queryTestedCopy,i+1));
              break;
          }
        }
      }
    }
    writer.close();
  }
}
