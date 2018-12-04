//Abstract class - Vehicle - is used to define the main characteristics of a Vehicle
import java.util.ArrayList;
public abstract class Vehicle {
    // protected field for the number of Spots used by the Vehicle
    protected static int numberOfSpots;
    // protected field for the size of the Vehicle
    protected VehicleSize vehicleSize;
    // protected field for the license number of the Vehicle
    protected String licenseNumber;
    // protected ArrayList for the available Parking Spots
    protected static ArrayList<ParkingSpot> parkingSpots = new ArrayList<ParkingSpot>();
    // getter method for the size of the Vehicle
    public VehicleSize getSize(){
        // return the size of the Vehicle
        return vehicleSize;
    } // end of getSize method
    // getter method for the number of spots used by a specific Vehicle
    public int getNumberOfSpots(){
        // return the number of spots used by a specific Vehicle
        return numberOfSpots;
    } // end of getNumberOfSpots method
    // method for adding a new ParkingSpor in the ArrayList
    public void parkSpots(ParkingSpot s){
        parkingSpots.add(s);
    } // end of parkSpot method
    // abstract method that tests if a Vehicle can fit in a spot or not
    public abstract boolean canFitSpot(ParkingSpot spt);
} // end of Vehicle class