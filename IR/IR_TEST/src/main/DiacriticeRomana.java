package main;

/**
 * Created by Ciprian Mihai on 4/01/2018.
 */

import java.util.ArrayList;
import java.util.Arrays;

public class DiacriticeRomana {
    public static ArrayList<String> FindAllPossibleDiacriticWords(String query) {
        char[] charArrayStrippedOfDiacritics = stripOfDiacritics(query);

        ArrayList<String> resultArray = new ArrayList<>();
        ArrayList<Pair> queriesToBeProcessed = new ArrayList<>();
        resultArray.add(new String(charArrayStrippedOfDiacritics));
        queriesToBeProcessed.add(new Pair(charArrayStrippedOfDiacritics,0));

        while (!queriesToBeProcessed.isEmpty())
        {
            Pair queryTestedPair = queriesToBeProcessed.get(0);
            queriesToBeProcessed.remove(0);

            for (int i = queryTestedPair.getIndex(); i < queryTestedPair.getQuery().length; i++)
            {
                char[] queryTestedPairCopy = Arrays.copyOf(queryTestedPair.getQuery(),queryTestedPair.getQuery().length);
                switch (queryTestedPair.getQuery()[i])
                {
                    case 'a':
                        queryTestedPairCopy[i]='ă';
                        resultArray.add(new String(queryTestedPairCopy));
                        queriesToBeProcessed.add(new Pair(queryTestedPairCopy,i+1));
                        char[] queryTestedCopyCopy = Arrays.copyOf(queryTestedPairCopy,queryTestedPairCopy.length);
                        queryTestedCopyCopy[i]='â';
                        resultArray.add(new String(queryTestedCopyCopy));
                        queriesToBeProcessed.add(new Pair(queryTestedCopyCopy,i+1));
                        break;
                    case 'i':
                        queryTestedPairCopy[i]='î';
                        resultArray.add(new String(queryTestedPairCopy));
                        queriesToBeProcessed.add(new Pair(queryTestedPairCopy,i+1));
                        break;
                    case 's':
                        queryTestedPairCopy[i]=537;
                        resultArray.add(new String(queryTestedPairCopy));
                        queriesToBeProcessed.add(new Pair(queryTestedPairCopy,i+1));
                        break;
                    case 't':
                        queryTestedPairCopy[i]=539;
                        resultArray.add(new String(queryTestedPairCopy));
                        queriesToBeProcessed.add(new Pair(queryTestedPairCopy,i+1));
                        break;
                    }
                }
            }
        return resultArray;
    }

    public static char[] stripOfDiacritics(String query){
        char[] charArrayStrippedOfDiacritics = query.toLowerCase().toCharArray();
        for(int i=0; i<query.length();i++) {
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
        return charArrayStrippedOfDiacritics;
    }
}