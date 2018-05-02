package ro.unibuc.fmi.fleamarket.fleamarket.security;

public class AuthUtils {

    public static String getCurrentUser(String token) {
        //TODO add date to token when extracting it, return only the user id
        if (token == null) {
            return "-1";
        }
        return JwtHandler.decryptFromJwt(token);

    }
}
