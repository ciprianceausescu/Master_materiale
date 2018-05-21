package com.company.shop.services;

import com.company.shop.model.Category;
import com.company.shop.model.UserDto;

public class UserService {

	public UserDto getCurrentLoggedInUser() {
		UserDto user = new UserDto();
		user.setName("Jhon");
		user.setCategory(Category.DIAMOND);
		return user;
	}
	
}
