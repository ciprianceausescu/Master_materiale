package ro.unibuc.fmi.fleamarket.fleamarket.security;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

import java.security.Key;

public class JwtHandler {

    private static JwtHandler instance;
    private static Key key;

    private JwtHandler() {
        key = KeysUtils.getSignKey();
    }

    public static JwtHandler getInstance() {
        if (instance == null) {
            instance = new JwtHandler();
        }

        return instance;
    }


    public String encryptInJwt(String toEncrypt) {
        if (key == null) {
            return null;
        }

        return Jwts.builder()
                .setSubject(toEncrypt)
                .signWith(SignatureAlgorithm.RS512, key)
                .compact();
    }

    public static String decryptFromJwt(String jwt) {
        if (jwt == null) {
            System.out.println("Unauthorized request!");
            return null;
        }
        return Jwts.parser().setSigningKey(key).parseClaimsJws(jwt).getBody().getSubject();
    }
}
