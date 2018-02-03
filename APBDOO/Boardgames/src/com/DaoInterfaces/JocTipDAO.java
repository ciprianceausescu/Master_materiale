package com.DaoInterfaces;

import java.util.List;

import com.Tables.JocTip;

public interface JocTipDAO {
	public List<JocTip> getAllTypes();
	public JocTip getGameType(long id);
}
