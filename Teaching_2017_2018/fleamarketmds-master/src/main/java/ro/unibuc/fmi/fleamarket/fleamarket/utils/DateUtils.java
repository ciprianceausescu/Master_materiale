package ro.unibuc.fmi.fleamarket.fleamarket.utils;

import ro.unibuc.fmi.fleamarket.fleamarket.domain.Product;

import java.text.SimpleDateFormat;
import java.util.Comparator;
import java.util.Date;

public class DateUtils {

    private static SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd-HH:mm:ss");

    public static Comparator<Product> getDateComparator() {
        return (o1, o2) -> o2.getId().compareTo(o1.getId());
    }

    public static String formatDate(Date date) {
        return dateFormatter.format(date);
    }

}
