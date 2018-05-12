package main;
import java.util.ArrayList;
import java.util.Arrays;

public class Diacritice {
    public static ArrayList<String> FindAllPossibleDiacriticWords(String query) {
        char[] charArrayStrippedOfDiacritics = stripOfDiacritics(query);

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