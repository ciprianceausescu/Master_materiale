package ro.unibuc.fmi.fleamarket.fleamarket.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.Person;

public interface PersonRepository extends JpaRepository<Person,Long> {

    @Query("select p from Person p " +
            "where p.email = :email")
    Person getPersonByEmail(@Param("email") String email);
}

