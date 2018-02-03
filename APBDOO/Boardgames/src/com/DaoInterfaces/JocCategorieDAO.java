package com.DaoInterfaces;

import java.util.List;

import com.Tables.JocCategorie;

public interface JocCategorieDAO {
	public JocCategorie getGameCategory(long id);
	public List<JocCategorie> getAllCategory();
}
