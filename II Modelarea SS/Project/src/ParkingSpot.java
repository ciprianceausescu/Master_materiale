//Class - ParkingSpot - is used to define a Parking Spot
public class ParkingSpot {
    // private field for the row
    private int row;
    // private field for the number of spots
    private int numOfSp;
    // private field for the size of Vehicle
    private VehicleSize sizeOfSp;
    // private field for the Level of Parking Spot
    private Level lv;
    // private field for the Vehicle of Parking Spot
    private Vehicle veh;
    // default constructor
    public ParkingSpot(){

    } // end of default constructor
    // overloading constructor
    public ParkingSpot(Level lvl, int ro, int no, VehicleSize vs){
        numOfSp = no;
        row = ro;
        lv = lvl;
        sizeOfSp = vs;
        lv = lvl;
    } // end of overloading constructor
    // getter for number of spots
    public int getNumOfSp(){
        // return statement
        return numOfSp;
    } // end of getSpotNumber method
    // getter for thr row
    public int getRow(){
        // return statement
        return row;
    } // end of getRow method
    // getter for the size of Vehicle
    public VehicleSize getSize(){
        // return statement
        return sizeOfSp;
    } // end of getSize method
    // method that checks if there is available spot
    public boolean isAvailable(){
        // return statement
        return (veh == null);
    } // end of isAvailable method
    // method that checks if vehicle fits or not
    public boolean fitVehicle(Vehicle veh){
        // return true value if the vehicle fits
        // return statement
        return veh.canFitSpot(this) && isAvailable();
    } // end of fitVehicle method
    // method parkVehicle is used to park the vehicle in the parking lot
    public boolean parkVehicle(Vehicle ve){
        if (!fitVehicle(ve)){
            // return statement
            return false;
        }
        veh = ve;
        veh.parkSpots(this);
        // return statement
        return true;
    } // end of parkVehicle method
    // method removeVehicle is used to remove a vehicle from the spot
    public boolean removeVehicle(){
        veh = null;
        return true;
    } // end of removeVehicle method
    // print out each type of parking spot and vehicle that parks
    public String toString(){
        // creating instance
        StringBuilder sb = new StringBuilder();
        if(!isAvailable()){
            if(veh.getSize() == VehicleSize.Bus){
                sb.append('B');
            } else if (veh.getSize() == VehicleSize.Car){
                sb.append('C');
            } else {
                sb.append('M');
            }
        } else {
            if(sizeOfSp == VehicleSize.Bus){
                sb.append('b');
            } else if (sizeOfSp == VehicleSize.Car){
                sb.append('c');
            } else {
                sb.append('m');
            }
        }
        // return statement
        return sb.toString();
    } // end of toString method
} // end of ParkingSpot class