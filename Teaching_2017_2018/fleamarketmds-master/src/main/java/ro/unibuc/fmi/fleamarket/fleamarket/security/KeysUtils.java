package ro.unibuc.fmi.fleamarket.fleamarket.security;


import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.security.*;
import java.security.cert.CertificateException;

public class KeysUtils {

    private static KeyStore loadKeyStore(final File keystoreFile,
                                         final String password, final String keyStoreType)
            throws KeyStoreException, IOException, NoSuchAlgorithmException,
            CertificateException {
        if (null == keystoreFile) {
            throw new IllegalArgumentException("Keystore url may not be null");
        }

        final URI keystoreUri = keystoreFile.toURI();
        final URL keystoreUrl = keystoreUri.toURL();
        final KeyStore keystore = KeyStore.getInstance(keyStoreType);
        InputStream is = null;
        try {
            is = keystoreUrl.openStream();
            keystore.load(is, null == password ? null : password.toCharArray());
        } finally {
            if (null != is) {
                is.close();
            }
        }
        return keystore;
    }


    private static KeyPair getkeyPair(final KeyStore keystore, final String alias, final String password) throws UnrecoverableKeyException, NoSuchAlgorithmException, KeyStoreException {
        final Key key = (PrivateKey) keystore.getKey(alias, password.toCharArray());

        final java.security.cert.Certificate cert = keystore.getCertificate(alias);
        final PublicKey publicKey = cert.getPublicKey();

        return new KeyPair(publicKey, (PrivateKey) key);
    }

    public static Key getSignKey(){
        KeyStore keyStore;
        Key key;
        try {
            ClassLoader classLoader = KeysUtils.class.getClassLoader();
            File file = new File(classLoader.getResource("keystore.jks").getFile());
            keyStore = loadKeyStore(file,"parola123", "jks");

            key = getkeyPair(keyStore, "server", "parola123").getPrivate();
            return key;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
