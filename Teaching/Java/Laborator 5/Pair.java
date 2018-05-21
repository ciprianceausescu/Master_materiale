public class Pair {
    private String s;
    private Integer i;

    public String getS() {
        return s;
    }

    public Integer getI() {
        return i;
    }

    public void setS(String s) {
        this.s = s;
    }

    public void setI(Integer i) {
        this.i = i;
    }

    public Pair(Integer i, String s)
    {
        this.i = i;
        this.s = s;
    }

    @Override
    public boolean equals(Object ob)
    {
        return s.equals(i.toString());
    }

    public String toString()
    {
        return s + " " + i.toString();
    }
}
