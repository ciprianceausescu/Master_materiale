package ro.unibuc.fmi.fleamarket.fleamarket.domain;


import ro.unibuc.fmi.fleamarket.fleamarket.domain.enumeration.Category;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.enumeration.ProductStatus;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;
import java.util.Optional;

@Entity
@Table(name = "product")
public class Product implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue
    private Long id;

    //@Size(min = 5, max = 30, message = "The product name must have between 5 and 30 characters.")
    @Column(name = "product_name")
    private String productName;

    @Column(name = "price")
    private Double price;

    //@Size(min = 30, max = 255, message = "The product description must have between 30 and 255 characters.")
    @Column(name = "description")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "category")
    private Category category;

    //@JsonIgnore
    @Column(name = "publish_date")
    private LocalDate publishDate;

    //@JsonIgnore
    @Enumerated(EnumType.STRING)
    @Column(name = "product_status")
    private ProductStatus productStatus;

    @Column(name ="picture")
    private String imageUrl;


    public String getImageUrl() {
        return imageUrl;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    public Product imageUrl(String imageUrl){
        this.imageUrl = imageUrl;
        return this;
    }

    @ManyToOne
    private Person person;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getProductName() {
        return productName;
    }

    public Product productName(String productName) {
        this.productName = productName;
        return this;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public Double getPrice() {
        return price;
    }

    public Product price(Double price) {
        this.price = price;
        return this;
    }

    public void setPrice(Double price) {
        this.price = price;
    }

    public String getDescription() {
        return description;
    }

    public Product description(String description) {
        this.description = description;
        return this;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Category getCategory() {
        return category;
    }

    public Product category(Category category) {
        this.category = category;
        return this;
    }

    public void setCategory(Category category) {
        this.category = category;
    }

    public LocalDate getPublishDate() {
        return publishDate;
    }

    public Product publishDate(LocalDate publishDate) {
        this.publishDate = publishDate;
        return this;
    }

    public void setPublishDate(LocalDate publishDate) {
        this.publishDate = publishDate;
    }

    public ProductStatus getProductStatus() {
        return productStatus;
    }

    public Product productStatus(ProductStatus productStatus) {
        this.productStatus = productStatus;
        return this;
    }

    public void setProductStatus(ProductStatus productStatus) {
        this.productStatus = productStatus;
    }

    public Person getPerson() {
        return person;
    }

    public Product person(Person person) {
        this.person = person;
        return this;
    }

    public void setPerson(Person person) {
        this.person = person;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Product product = (Product) o;
        if (product.getId() == null || getId() == null) {
            return false;
        }
        return Objects.equals(getId(), product.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(getId());
    }

    @Override
    public String toString() {
        return "Product{" +
                ", productName='" + getProductName() + "'" +
                ", price='" + getPrice() + "'" +
                ", description='" + getDescription() + "'" +
                ", category='" + getCategory() + "'" +
                ", publishDate='" + getPublishDate() + "'" +
                ", productStatus='" + getProductStatus() + "'" +
                "}";
    }

}