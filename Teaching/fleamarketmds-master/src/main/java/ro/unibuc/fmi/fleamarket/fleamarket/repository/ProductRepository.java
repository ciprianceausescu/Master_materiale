package ro.unibuc.fmi.fleamarket.fleamarket.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.Product;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.enumeration.Category;

import java.util.List;

public interface ProductRepository extends JpaRepository<Product,Long> {

    @Query("select p from Product p " +
            "where p.category = :category AND p.productStatus = 'ENABLED' ")
    List<Product> getProductsListByCategory(@Param("category") Category category);

    @Query("select p from Product p " +
            "where p.person.id = :personId")
    List<Product> getMyProducts(@Param("personId") Long personId);

    @Query("select p from Product p " + "where UPPER(p.productName) LIKE CONCAT('%',UPPER(:word),'%')  " +
            "AND p.productStatus = 'ENABLED' " )
    List<Product> getProductsListBySearch(@Param("word") String word);
}


