package com.company.shop.persistance;

import java.util.ArrayList;
import java.util.List;

import com.company.shop.model.ProductDto;

public class ProductDao {

	public List<ProductDto> queryAvailableProducts() {
		List<ProductDto> availableProducts = new ArrayList<>();
		
		ProductDto product1 = new ProductDto();
		product1.setId(1212121212);
		product1.setLabel("R2D2");
		product1.setDescription("Howse cleaning robot.");
		
		ProductDto product2 = new ProductDto();
		product2.setId(1313131313);
		product2.setLabel("Night Guardian");
		product2.setDescription("Flying drone with infrared vision.");
		
		ProductDto product3 = new ProductDto();
		product3.setId(1001001001);
		product3.setLabel("Little Joe");
		product3.setDescription("Nanotechnology super small humanoid robot.");
		
		availableProducts.add(product1);
		availableProducts.add(product2);
		availableProducts.add(product3);
		
		return availableProducts;
	}
	
}
