package ro.unibuc.fmi.fleamarket.fleamarket.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "person", uniqueConstraints = {@UniqueConstraint(columnNames = {"email"})})
public class Person implements Serializable{
    //private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Long id;

    @NotNull
    @Pattern(regexp = ".*\\@.*\\..*", message = "This does not appear to be a valid email address")
    @Column(name = "email", unique = true)
    private String email;

    @NotNull
    @Column(name="password")
    private String hashedPassword;

    @NotNull
    @Size(min = 10, max = 10, message = "The phone number must have exactly 10 numbers.")
    @Column(name = "phone_number")
    private String phoneNumber;

    @Column(name = "rating")
    private Integer rating;

    @Column(name = "nr_of_posts")
    private Integer nrOfPosts = 0;

    @Column(name = "profile_pic")
    private String profilePic;


    @OneToMany(mappedBy = "person")
    @JsonIgnore
    private Set<Product> products = new HashSet<>();

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getEmail() {
        return email;
    }

    public Person email(String email) {
        this.email = email;
        return this;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getHashedPassword() {
        return hashedPassword;
    }

    public Person hashedPassword(String hashedPassword) {
        this.hashedPassword = hashedPassword;
        return this;
    }

    public void setHashedPassword(String hashedPassword) {
        this.hashedPassword = hashedPassword;
    }

    public String getPhoneNumber() {
        return phoneNumber;
    }

    public Person phoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
        return this;
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public Integer getRating() {
        return rating;
    }


    public Integer getNrOfPosts() {
        return nrOfPosts;
    }

    public Person nrOfPosts(Integer nrOfPosts) {
        this.nrOfPosts = nrOfPosts;
        return this;
    }

    public void setNrOfPosts(Integer nrOfPosts) {
        this.nrOfPosts = nrOfPosts;
    }

    public String getProfilePic() {
        return profilePic;
    }

    public Person profilePic(String profilePic) {
        this.profilePic = profilePic;
        return this;
    }

    public void setProfilePic(String profilePic) {
        this.profilePic = profilePic;
    }

    public Set<Product> getProducts() {
        return products;
    }

    public Person products(Set<Product> products) {
        this.products = products;
        return this;
    }

    public Person addProduct(Product product) {
        this.products.add(product);
        product.setPerson(this);
        return this;
    }

    public Person removeProduct(Product product) {
        this.products.remove(product);
        product.setPerson(null);
        return this;
    }

    public void setProducts(Set<Product> products) {
        this.products = products;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Person person = (Person) o;
        if (person.getId() == null || getId() == null) {
            return false;
        }
        return Objects.equals(getId(), person.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(getId());
    }

    @Override
    public String toString() {
        return "Person{" +
                "id=" + getId() +
                ", email='" + getEmail() + "'" +
                ", phoneNumber='" + getPhoneNumber() + "'" +
                "}";
    }
}
