//Class - Car - is used to define the Car Vehicle
public class Car extends Vehicle{
    // constructor of Car class
    public Car(){
        numberOfSpots = 1;
        vehicleSize = VehicleSize.Car;
    } // end of constructor
    // method that tests if a Car can fit in a spot or not
    public boolean canFitSpot(ParkingSpot parkingSpot){
        // return true if it is a Car or Bus spot
        return parkingSpot.getSize() == VehicleSize.Bus || parkingSpot.getSize() == VehicleSize.Car;
    } // end of canFitSpot method
    public String toString(){
        return "C";
    }
} // end of Car class