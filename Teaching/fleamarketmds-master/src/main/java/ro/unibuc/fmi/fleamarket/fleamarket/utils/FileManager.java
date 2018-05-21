package ro.unibuc.fmi.fleamarket.fleamarket.utils;


import java.io.File;

public class FileManager {

    public static String getImagePath(String imageName) {
        String desktopPath = System.getProperty("user.home") + "/Desktop";

        String imageDirPath = desktopPath + "/images/";
        new File(imageDirPath).mkdir(); //create Desktop/images if doesn't exist

        String newImagePath = imageDirPath + imageName;
        return newImagePath;
    }
}
