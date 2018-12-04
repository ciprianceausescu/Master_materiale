//Class - Bus - is used to define the Bus Vehicle
public class Bus extends Vehicle{
    // constructor of Bus class
    public Bus(){
        numberOfSpots = 5;
        vehicleSize = VehicleSize.Bus;
    } // end of constructor
    // method that tests if a Bus can fit in a spot or not
    public boolean canFitSpot(ParkingSpot parkingSpot){
        // return true if it is a Bus spot
        return parkingSpot.getSize() == VehicleSize.Bus;
    } // end of canFitSpot method
    public String toString(){
        return "B";
    }
} // end of Bus class