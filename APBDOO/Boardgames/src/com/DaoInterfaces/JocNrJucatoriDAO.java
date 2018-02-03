package com.DaoInterfaces;

import java.util.List;

import com.Tables.JocNrJucatori;

public interface JocNrJucatoriDAO {
	public List<JocNrJucatori> getAllNumberPlayers();
	public JocNrJucatori getNumberPlayers(long id);
}
