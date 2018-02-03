package com.DaoInterfaces;

import java.util.List;

import com.Tables.Adresa;

public interface AdresaDAO {
	public Adresa getAddress(long id);
	public List<Adresa> getAllAddresses();
}
