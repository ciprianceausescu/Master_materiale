
import java.io.File;
import java.io.FileFilter;

public class LuceneFileFilter implements FileFilter {

  @Override
  public boolean accept(File pathname) {
    boolean accept = pathname.getName().toLowerCase().endsWith(".txt") ||
        pathname.getName().toLowerCase().endsWith(".docx") ||
        pathname.getName().toLowerCase().endsWith(".doc") ||
        pathname.getName().toLowerCase().endsWith(".html") ||
        pathname.getName().toLowerCase().endsWith(".pdf");
    return accept;
  }
}