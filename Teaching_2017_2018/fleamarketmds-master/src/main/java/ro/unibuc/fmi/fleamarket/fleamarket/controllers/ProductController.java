package ro.unibuc.fmi.fleamarket.fleamarket.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.Product;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.enumeration.Category;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.enumeration.ProductStatus;
import ro.unibuc.fmi.fleamarket.fleamarket.repository.PersonRepository;
import ro.unibuc.fmi.fleamarket.fleamarket.repository.ProductRepository;
import ro.unibuc.fmi.fleamarket.fleamarket.security.AuthUtils;
import ro.unibuc.fmi.fleamarket.fleamarket.utils.DateUtils;
import ro.unibuc.fmi.fleamarket.fleamarket.utils.FileManager;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.json.Json;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.*;
import java.util.*;
import static java.time.LocalDate.now;


@RestController
@RequestMapping("/products")
public class ProductController {

    @Autowired
    private ProductRepository productRepository;

    @Autowired
    private PersonRepository personRepository;

    @RequestMapping(method = RequestMethod.GET,produces = "application/json")
    public List<Product> products(){
        return productRepository.findAll();
    }

    @RequestMapping(method = RequestMethod.GET,produces = "application/json", value="/latest6Products")
    public List<Product> latest6Products(){
        List<Product> listOfProducts = productRepository.findAll();
        listOfProducts.sort(DateUtils.getDateComparator());
        int endIndex = 6;
        if (listOfProducts.size() < 6 ) {
            endIndex = listOfProducts.size();
        }
        return listOfProducts.subList(0, endIndex);
    }

    @RequestMapping(value="/my", method = RequestMethod.GET,produces = "application/json")
    public List<Product> getUserProducts(@RequestHeader(value = "Authorization", defaultValue = "") String auth){
        Long userId = Long.valueOf(AuthUtils.getCurrentUser(auth));
        if (userId == -1) {
            System.out.println("Unauthorized!!!");
            return null;
        }
        List<Product> listOfProducts = productRepository.getMyProducts(userId);
        List<Product> enabledProducts = new ArrayList<Product>();
        for (Product p : listOfProducts) {
            if (p.getProductStatus() == ProductStatus.ENABLED) {
                enabledProducts.add(p);
            }
        }
        return enabledProducts;
    }

    @RequestMapping(value="/history", method = RequestMethod.GET,produces = "application/json")
    public List<Product> getDisabledProducts(@RequestHeader(value = "Authorization", defaultValue = "") String auth){
        Long userId = Long.valueOf(AuthUtils.getCurrentUser(auth));
        if (userId == -1) {
            System.out.println("Unauthorized!!!");
            return null;
        }
        List<Product> listOfProducts = productRepository.getMyProducts(userId);
        List<Product> disabledProducts = new ArrayList<Product>();
        for (Product p : listOfProducts) {
            if (p.getProductStatus() == ProductStatus.DISABLED) {
                disabledProducts.add(p);
            }
        }
        return disabledProducts;
    }

    @RequestMapping(method = RequestMethod.GET, value="/{id}", produces = "application/json")
    public Optional<Product> getProductById(@PathVariable("id") Long id){
        return productRepository.findById(id);
    }

    @RequestMapping(method = RequestMethod.POST, consumes = "application/json")
    public String saveProduct(@RequestBody @Valid Product product,
                              @RequestHeader(value="Authorization", defaultValue = "") String auth){
        Long userId   = Long.valueOf(AuthUtils.getCurrentUser(auth));
        if(userId != -1){
            product.setId(null);
            product.setPublishDate(now());
            product.setProductStatus(ProductStatus.ENABLED);
            product.setPerson(personRepository.findById(userId).get());
            productRepository.save(product);
            return Json
                    .createObjectBuilder()
                    .add("productId", product.getId())
                    .build()
                    .toString();
        }
        else {
            System.out.println("Please sing in, after that you can create an product");
            return "";
        }
    }

    @RequestMapping(method = RequestMethod.DELETE, value="/{id}")
    public void deleteProduct(@PathVariable("id") Long id,
                              @RequestHeader(value="Authorization", defaultValue = "") String auth){
        Product product = productRepository.findById(id).get();
        productRepository.delete(product);
    }

    @RequestMapping(method = RequestMethod.PUT, value="/{id}")
    public String editProduct(@RequestBody @Valid Product editProduct ,@PathVariable("id") Long id,
                              @RequestHeader(value="Authorization", defaultValue = "") String auth) {
        Long userId = Long.valueOf(AuthUtils.getCurrentUser(auth));
        if (userId != -1) {
            editProduct.setId(id);
            editProduct.setPublishDate(now());
            editProduct.setPerson(personRepository.findById(userId).get());
            editProduct.setImageUrl((productRepository.findById(id)).get().getImageUrl());
            productRepository.saveAndFlush(editProduct);
            return Json
                    .createObjectBuilder()
                    .add("productId", editProduct.getId())
                    .build()
                    .toString();
        } else {
            System.out.println("Please sing in, after that you can edit an product");
            return "";
        }
    }
    @RequestMapping(method = RequestMethod.GET, value="/category/{category}", produces = "application/json")
    public List<Product> getProductsListByCategory(@PathVariable("category") Category category){
        return productRepository.getProductsListByCategory(category);
    }

    @RequestMapping(method = RequestMethod.GET, value="/search/{word}", produces = "application/json")
    public List<Product> getProductsListBySearch(@PathVariable("word") String word){
        return productRepository.getProductsListBySearch(word);
    }

        @RequestMapping(value="/upload", method = RequestMethod.POST,produces = "application/json")
        public String uploadNewImage(@RequestParam(value="file", required=true) MultipartFile file,
                @RequestHeader(value="Authorization", defaultValue = "") String auth) {

            if (auth == null || auth.equals("")) {
                return Json.createObjectBuilder().add("error", "Forbidden").build().toString();
            }

            String originalImageName = file.getOriginalFilename();
            String imageName = AuthUtils.getCurrentUser(auth) + originalImageName;


            String imageAbsolutePath = FileManager.getImagePath(imageName);
            System.out.println(imageAbsolutePath);
            try {
                File f = new File(imageAbsolutePath);
                f.createNewFile();
                file.transferTo(f);
            } catch (IllegalStateException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }
            return Json.createObjectBuilder().add("imageUrl", imageName).build().toString();
        }

        @RequestMapping(value="/images/{imageName:.+}", method = RequestMethod.GET)
        public void getImage(HttpServletRequest req,
                HttpServletResponse resp,
                @PathVariable("imageName") String imageName) {

            String imageAbsolutePath = FileManager.getImagePath(imageName);
            System.out.println(imageAbsolutePath);

            // retrieve mimeType dynamically
            ServletContext cntx= req.getServletContext();
            String mime = cntx.getMimeType(imageAbsolutePath);
            if (mime == null) {
                resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                return;
            }

            resp.setContentType(mime);
            File file = new File(imageAbsolutePath);
            resp.setContentLength((int)file.length());

            FileInputStream in = null;
            OutputStream out = null;
            try {
                in = new FileInputStream(file);
                out = resp.getOutputStream();

                // Copy the contents of the file to the output stream
                byte[] buf = new byte[1024];
                int count = 0;
                while ((count = in.read(buf)) >= 0) {
                    out.write(buf, 0, count);
                }
                out.close();
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
                return;
            }

        }
    }

