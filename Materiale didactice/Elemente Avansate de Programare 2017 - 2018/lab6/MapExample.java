package lab8.collections;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Exemplu de lucru cu HashMap.
 */
public class MapExample {

    public static void main(String[] args) {

        Map<String, Integer> map = new HashMap<String, Integer>();
        map.put("abc", 1);
        map.put("bdc", 2);
        map.put("abc", 3);

        Iterator<String> iterator = map.keySet().iterator();
        Iterator<Integer> it2 = map.values().iterator();
        Iterator<Map.Entry<String, Integer>> it3 = map.entrySet().iterator();

        // Iteram cheile hashMap-ului
        while(iterator.hasNext()) {
            String key = iterator.next();
            System.out.println(key);
        }

        // Iteram valorile
        while(it2.hasNext()) {
            int value = it2.next();
            System.out.println(value);
        }

        // Iteram intrarile in hashMap, care sunt instante ale clasei Map.Entry.
        while(it3.hasNext()) {
            Map.Entry<String, Integer> entry = it3.next();
            String key = entry.getKey();
            Integer value = entry.getValue();
            System.out.println(key + " " + value);
        }
    }
}
