package com.DaoInterfaces;

import java.util.List;

import com.Tables.Angajat;
import com.Tables.Joc;

public interface JocDAO {
	   public List<Joc> getAllGames();
	   public Joc getGame(int id);
	   public void updateGame(Joc joc);
	   public List<Joc> searchGame(String nume);
	   public void deleteGame(int id);
	   public void addGame(Joc joc);
}
