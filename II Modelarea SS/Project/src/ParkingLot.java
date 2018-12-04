//Class - ParkingLot - is used to define a Parking Lot
public class ParkingLot {
    // private field for Array of Levels
    private Level[] levels;
    // private field for number of levels
    private final int NUM_OF_LEVELS = 5;
    // private field for number of spots
    private final int NUM_OF_SPOTS = 30;
    // constructor
    public ParkingLot(){
        levels = new Level[NUM_OF_LEVELS];
        for (int count = 0; count < NUM_OF_LEVELS; count++){
            levels[count] = new Level(count, NUM_OF_SPOTS);
        } // end of for loop
    } // end of constructor
    // method for parking the Vehicle
    public boolean parkVehicle(Vehicle veh){
        for (int count = 0; count < levels.length; count++){
            if (levels[count].parkVehicle(veh)){
                // return statement
                return true;
            }
        } // end of for loop
        // return statement
        return false;
    } // end of parkVehicle method
    // print out the result in string style
    // method for remove a Vehicle
    public boolean removeVehicle(Vehicle veh, int level, int position){
        return levels[level].freeSpot(veh, position);
    }
    public String toString(){
        // creating instance
        StringBuilder sb = new StringBuilder();
        // print out parking lot
        for (int count = 0; count < NUM_OF_LEVELS; count++){
            sb.append("Level " + count + ": " + levels[count] + "\n");
        } // end of for loop
        // return statement
        return sb.toString();
    } // end of toString method
} // end of ParkingLot class