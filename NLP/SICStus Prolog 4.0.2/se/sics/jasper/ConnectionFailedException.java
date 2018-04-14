// ************************************************************************
// * Filename:	   ConnectionFailedException.java
// * Author:	   Jesper Eskilson <jojo@sics.se>
// * Created:	   Thu May 13 16:39:18 1999
// * Description: 
// * Last-Update:  Time-stamp: <1999-05-13 1845 jojo>
// ************************************************************************

package se.sics.jasper;

/** <b>This code is unsupported and unfinished. It is provided "as is" for
 * demontration purposes only.</b> The supported interface between Java
 * and SICStus is the {@link se.sics.jasper.SICStus SICStus} class and related classes.<p>
 * @deprecated Only intended as an example
 */
public class ConnectionFailedException extends Exception
{
  ConnectionFailedException(String msg)
  {
    super(msg);
  }
}

