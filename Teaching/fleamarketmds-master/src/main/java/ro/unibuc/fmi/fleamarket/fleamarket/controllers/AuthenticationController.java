package ro.unibuc.fmi.fleamarket.fleamarket.controllers;



import org.mindrot.jbcrypt.BCrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import ro.unibuc.fmi.fleamarket.fleamarket.domain.Person;
import ro.unibuc.fmi.fleamarket.fleamarket.repository.PersonRepository;
import ro.unibuc.fmi.fleamarket.fleamarket.security.AuthUtils;
import ro.unibuc.fmi.fleamarket.fleamarket.security.JwtHandler;

import javax.json.*;
import java.io.StringReader;

@RestController
@RequestMapping("/api")
public class AuthenticationController {

    @Autowired
    private PersonRepository personRepository;

    private JwtHandler jwtHandler = JwtHandler.getInstance();

    @RequestMapping(value = "/authenticate", method = RequestMethod.POST,
            consumes = "application/json", produces = "application/json")
    public String auth(@RequestBody String jsonRequestBody) {
        JsonObject jsonObject = Json.createReader(new StringReader(jsonRequestBody)).readObject();

        /* check if request contains email/pass */
        String email = jsonObject.getString("email");
        String password = jsonObject.getString("password");

        if (email != null && password != null) {
            Person user = personRepository.getPersonByEmail(email);
            String hashed = user.getHashedPassword();

            /* check if hashed passwords are matching */
            if (BCrypt.checkpw(password, hashed)) {
                System.out.println("Passwords match, starting jwt encryption");
                String authToken = jwtHandler.encryptInJwt(user.getId().toString());

                if (authToken != null) {
                    return Json.createObjectBuilder().add("token", authToken).build().toString();
                }
            }
        }
        return null;
    }

    @RequestMapping(value = "/register", method = RequestMethod.POST,
            consumes = "application/json", produces = "application/json")
    public String register(@RequestBody String jsonRequestBody) {

        JsonObject jsonObject = Json.createReader(new StringReader(jsonRequestBody)).readObject();

        String email = jsonObject.getString("email");

        if (personRepository.getPersonByEmail(email) == null) {
            String phone = jsonObject.getString("phone");
            String password = jsonObject.getString("password");

            Person person = new Person();
            person.setEmail(email);
            person.setPhoneNumber(phone);
            person.setHashedPassword(BCrypt.hashpw(password, BCrypt.gensalt()));

            personRepository.save(person);

            return Json.createObjectBuilder().add("created", "true").build().toString();
        } else {
            System.out.println("Account already created!");
            return Json.createObjectBuilder().add("not_created", "true").build().toString();
        }
    }

    @RequestMapping(value = "/whoami", method = RequestMethod.GET, produces = "application/json")
    public String whoAmI(@RequestHeader(value="Authorization", defaultValue = "") String auth) {
        String loggedUser = "-1"; // guest user

        if (auth != null && !auth.equals("")) {
            loggedUser = AuthUtils.getCurrentUser(auth);
        }
        return Json.createObjectBuilder().add("loggedin", loggedUser).build().toString();
    }
}
