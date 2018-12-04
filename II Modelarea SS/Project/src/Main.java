//Class - Main - is used to test the design of the Parking Lot
import java.util.Scanner;
public class Main {
    public static void main(String[] args) {
        // TODO Auto-generated method stub
        // creating instance
        ParkingLot pLot = new ParkingLot();
        // creating Vehicle factory
        VehicleFactory vehicleFactory = new VehicleFactory();
        // variable declaration
        String inp = "";
        // scanner input
        Scanner input = new Scanner (System.in);
        // output
        // print out an empty parking lot
        System.out.println("The empty parking lot\n");
        System.out.println(pLot.toString());
        // input
        // ask user to type in the vehicle that he wants to parkVehicle
        System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
        System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
        System.out.println("Please enter 0 to exit.");
        inp = input.next();
        // continue asking user for input until user enters zero
        while(!inp.equals("0")){
            if (inp.equals("bus") || inp.equals("Bus")){
                // creating instance
                Vehicle b = vehicleFactory.getVehicle("bus");
                if(pLot.parkVehicle(b)){
                    // vehicle parked
                    // success message
                    System.out.println("\nThe bus has been parked successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to enter other input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                } else {
                    // failure message
                    System.out.println("\nParking Failed: Bus spots are full");
                    // ask user to re-enter their inputs
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            } else if(inp.equals("car") || inp.equals("Car")){
                // creating instance
                Vehicle c = vehicleFactory.getVehicle("car");
                if(pLot.parkVehicle(c)){
                    // vehicle parked
                    // success message
                    System.out.println("\nThe car has been parked successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to enter other input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                } else {
                    // failure message
                    System.out.println("\nParking Failed: large spots are full");
                    // ask user to re-enter their inputs
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            } else if(inp.equals("motor") || inp.equals("Motor")){
                // creating instance
                Vehicle m = vehicleFactory.getVehicle("motorcycle");
                if(pLot.parkVehicle(m)){
                    // vehicle parked
                    // success message
                    System.out.println("\nThe motorcycle has been parked successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to enter other input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                } else {
                    // failure message
                    System.out.println("\nParking Failed: Motorcycle spots are full");
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            }
            else if(inp.equals("fbus") || inp.equals("Fbus")){
                // creating instance
                Vehicle m = vehicleFactory.getVehicle("bus");
                System.out.println("Level number: ");
                int level = Integer.parseInt(input.next());
                System.out.println("Position at level: ");
                int position = Integer.parseInt(input.next());
                if(pLot.removeVehicle(m, level,position)) {
                    // success message
                    System.out.println("\nThe bus left the parking spot successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
                else{
                    // failure message
                    System.out.println("\nThe bus cannot left the parking spot.");
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            }
            else if(inp.equals("fcar") || inp.equals("Fcar")){
                // creating instance
                Vehicle m = vehicleFactory.getVehicle("car");
                System.out.println("Level number: ");
                int level = Integer.parseInt(input.next());
                System.out.println("Position at level: ");
                int position = Integer.parseInt(input.next());
                if(pLot.removeVehicle(m, level,position)) {
                    // success message
                    System.out.println("\nThe car left the parking spot successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
                else{
                    // failure message
                    System.out.println("\nThe car cannot left the parking spot.");
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            }
            else if(inp.equals("fmoto") || inp.equals("Fmoto")){
                // creating instance
                Vehicle m = vehicleFactory.getVehicle("motorcycle");
                System.out.println("Level number: ");
                int level = Integer.parseInt(input.next());
                System.out.println("Position at level: ");
                int position = Integer.parseInt(input.next());
                if(pLot.removeVehicle(m, level,position)) {
                    // success message
                    System.out.println("\nThe motorcycle left the parking spot successfully.");
                    // print out parking lot
                    System.out.println(pLot.toString());
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
                else{
                    // failure message
                    System.out.println("\nThe motorcycle cannot left the parking spot.");
                    // ask user to re-enter input
                    System.out.println("Please enter bus, car, or motor to parkVehicle a vehicle.");
                    System.out.println("Please enter fbus, fcar, or fmotor to remove a vehicle.");
                    System.out.println("Please enter 0 to exit.");
                    inp = input.next();
                }
            }
            else {
                // failure message
                System.out.println("\nInvalid input.");
                // ask user to re-enter input
                System.out.print("\nPlease enter bus, car, or motorcycle to parkVehicle a vehicle (enter 0 to exit): ");
                inp = input.next();
            }
            // terminating program
            if(inp.equals("0")){
                System.out.print("\nProgram terminated");
                break;
            }
        } // end of while loop
    } // end of main method
} // end of Main class