//Class - Motorcycle - is used to define the Motorcycle Vehicle
public class Motorcycle extends Vehicle{
    // constructor of Motorcycle class
    public Motorcycle(){
        numberOfSpots = 1;
        vehicleSize = VehicleSize.Motorcycle;
    } // end of constructor
    // method that tests if a Motorcycle can fit in a spot or not
    public boolean canFitSpot(ParkingSpot parkingSpot){
        // return true if it is a Motorcycle, Car or Bus spot
        return parkingSpot.getSize() == VehicleSize.Motorcycle || parkingSpot.getSize() == VehicleSize.Car || parkingSpot.getSize() == VehicleSize.Bus;
    } // end of canFitSpot method
    public String toString(){
        return "M";
    }
} // end of Motorcycle class