//Class - VehicleFactory - is used to construct and return new Vehicle objects based on a parameter
public class VehicleFactory {
    // method that takes as parameter a String - vehicle type and if it is a valid Vehicle it returns an instance of that Vehicle type
    public Vehicle getVehicle(String vehicleType){
        if(vehicleType == null){
            return null;
        }
        if(vehicleType.equalsIgnoreCase("MOTORCYCLE")){
            return new Motorcycle();
        }
        if(vehicleType.equalsIgnoreCase("CAR")){
            return new Car();
        }
        if(vehicleType.equalsIgnoreCase("BUS")){
            return new Bus();
        }
        return null;
    }
}
