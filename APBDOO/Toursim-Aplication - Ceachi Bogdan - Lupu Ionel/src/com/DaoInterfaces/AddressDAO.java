package com.DaoInterfaces;

import java.util.List;

import com.Tables.Address;
import com.Tables.City;
import com.Tables.Country;

public interface AddressDAO {
	   public List<Address> getAllAddress();
	   public Address getAddress(int id);
	   
	   
	   public City getCity(int city_id);
	   
	   
	   
	   public void updateAddress(Address address);
	   public void deleteAddress(Address address);

}
