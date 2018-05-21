package com.company.shop.services;

import java.util.List;

import com.company.shop.model.ProductDto;
import com.company.shop.persistance.ProductDao;

public class ProductService {
	
	ProductDao productDao = new ProductDao();

	public List<ProductDto> getListAvailableProducts() {
		return productDao.queryAvailableProducts();	
	}
	
}
