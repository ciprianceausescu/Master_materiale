package main;

import org.apache.lucene.index.FieldInvertState;
import org.apache.lucene.search.MultiCollectorManager;
import org.apache.lucene.search.similarities.TFIDFSimilarity;
import org.apache.lucene.util.BytesRef;

import java.text.Normalizer;
import java.util.List;
import java.util.stream.Collectors;

import static java.lang.Math.log;
import static java.lang.Math.sqrt;

class TFIDF extends TFIDFSimilarity {

    @Override
    public float coord(int i, int i1) {
        return 0;
    }

    @Override
    public float queryNorm(float v) {
        return 0;
    }

    @Override
    public float tf(float v) {
        return (float)sqrt(v);
    }

    @Override
    public float idf(long l, long l1) {
        return  (float)log(l1/(l+1)) + 1;
    }

    @Override
    public float lengthNorm(FieldInvertState fieldInvertState) {
        return 0;
    }

    @Override
    public float decodeNormValue(long l) {
        return 0;
    }

    @Override
    public long encodeNormValue(float v) {
        return 0;
    }

    @Override
    public float sloppyFreq(int i) {
        return 0;
    }

    @Override
    public float scorePayload(int i, int i1, int i2, BytesRef bytesRef) {
        return 0;
    }

    public static double tf(List<String> doc, String term) {
        return (double) doc.stream()
                           .map(TFIDF::removeAccents)
                           .filter(s->s.toLowerCase().contains(term.toLowerCase()))
                           .collect(Collectors.toList()).size() / doc.size();
    }

    public static String removeAccents(String text) {
        return text == null ? null :
                Normalizer.normalize(text, Normalizer.Form.NFD)
                        .replaceAll("\\p{InCombiningDiacriticalMarks}+", "");
    }
}