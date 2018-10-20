package lab3;

/**
 * Encapsuleaza un dreptunghi.
 */
public class Dreptunghi {
    double lung, lat;

    public Dreptunghi() {
        lung = 1;
        lat = 1;
    }

    public Dreptunghi(double lung1, double lat1) {
        lung = lung1;
        lat = lat1;
    }

    public double getLung() {
        return lung;
    }

    public void setLung(double lung) {
        this.lung = lung;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    double arie() {
        return lung * lat;
    }

    boolean maiMare(Dreptunghi d) {
        return arie() < d.arie();
    }

    public void afisare() {
        System.out.println("numElem " + lung + ",latime " + lat);
    }
}
