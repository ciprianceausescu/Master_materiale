package com.DaoInterfaces;

import java.util.List;

import com.Tables.JocProducator;

public interface JocProducatorDAO {
	public List<JocProducator> getAllProducer();
	public JocProducator getGameProducer(long id);
}
