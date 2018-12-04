//Class - Level - is used to define the Level of a ParkingLot
public class Level {
    // private field for the Array of Parking Spots
    private ParkingSpot[] parkingSpots;
    // private field for the number of available spots
    private int availableSpots = 0;
    // private field for the number of spots per row
    private static final int SPOTS_PER_ROW = 10;
    // default constructor
    public Level(){
    } // end of default constructor
    // overloading constructor
    public Level(int flr, int numSpts){
        // variable declaration
        int smSpts = 0;
        int medSpts = 0;
        int lgSpts = 0;
        int spots = 0;
        int row = 0;
        availableSpots = numSpts;
        parkingSpots = new ParkingSpot[numSpts];
        // calculation
        smSpts = numSpts / 4;
        lgSpts = numSpts / 4;
        medSpts = numSpts - smSpts - lgSpts;
        spots = medSpts + lgSpts;
        for (int count = 0; count < numSpts; count++){
            // creating instance
            VehicleSize vehicleSize = VehicleSize.Motorcycle;
            if (count < lgSpts){
                vehicleSize = VehicleSize.Bus;
            }
            else if (count < spots){
                vehicleSize = VehicleSize.Car;
            }
            // calculation
            row = count / SPOTS_PER_ROW;
            parkingSpots[count] = new ParkingSpot(this, row, count, vehicleSize);
        } // end of for loop
    } // end of overloading constructor
    // tells program how many available spots are there
    public int availableSpots(){
        // return statement
        return availableSpots;
    } // end of availableSpots method
    public boolean parkVehicle(Vehicle veh){
        // variable declaration and initialization
        int spotNumber = findAvailableSpots(veh);
        // check if there is still suitable spot left
        if (availableSpots() < veh.getNumberOfSpots()){
            // return statement
            return false;
        }
        if (spotNumber < 0){
            // return statement
            return false;
        }
        // return statement
        return parkAtSpot(spotNumber, veh);
    } // end of parkVehicle method
    // return true if a vehicle parks successfully
    private boolean parkAtSpot(int numb, Vehicle veh){
        // parkVehicle a vehicle starting at the spot spotNumber, and continuing until vehicle.spotsNeeded
        // variable declaration
        boolean succ = true;
        for (int count = numb; count < (Vehicle.numberOfSpots + numb); count++){
            succ = succ & parkingSpots[count].parkVehicle(veh);
        } // end of for loop
        // calculation
        availableSpots = availableSpots - Vehicle.numberOfSpots;
        // return statement
        return succ;
    } // end of parkAtSpot method
    // find available spot if there is any
    private int findAvailableSpots(Vehicle veh){
        // variable declaration
        int sptNeeded = veh.getNumberOfSpots();
        int lastR = -1;
        int sptsFd = 0;
        int sptMOne = 0;
        for (int count = 0; count < parkingSpots.length; count++){
            // creating instance
            ParkingSpot spt = parkingSpots[count];
            if (lastR != spt.getRow()){
                sptsFd = 0;
                lastR = spt.getRow();
            }
            if (spt.fitVehicle(veh)){
                sptsFd++;
            } else {
                sptsFd = 0;
            }
            sptMOne = sptNeeded - 1;
            if (sptsFd == sptNeeded){
                return (count - sptMOne);
            }
        } // end of for loop
        // return statement
        return -1;
    } // end of findAvailableSpots method
    // this method free up spot
    public boolean freeSpot(Vehicle veh, int position){
        if(parkingSpots[position].toString().equals(veh.toString())){
            boolean succ = true;
            for (int count = position; count < (Vehicle.numberOfSpots + position); count++){
                succ = succ & parkingSpots[count].removeVehicle();
                // calculation
                availableSpots++;
            } // end of for loop
            return true;
        }
        return false;
    } // end of freeSpot method
    // print out in string style
    public String toString(){
        // creating instance
        StringBuilder sb = new StringBuilder();
        for (int count = 0; count < parkingSpots.length; count++){
            if (count % 10 == 0){
                sb.append(" ");
            }
            sb.append(parkingSpots[count]);
        } // end of for loop
        // return statement
        return sb.toString();
    } // end of toString method
} // end of Level class