/**
 * PBTest.java
 *
 *
 * Created: Tue Oct 07 17:39:39 2003
 *
 * @Author  : Joakim Eriksson & Niclas Finne
 * @version 1.0
 */
import se.sics.prologbeans.*;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class PBTest implements Runnable {
  private static int error = 0;
  private int port = -1;
  private Process process;
  private boolean isShutdown = false;

  public void run() {
    try {
      String prolog = System.getProperty("se.sics.prologbeans.prolog",
					  "sicstus");
      // [PD] This is portable (i.e. works both for SICStus and Quintus)
      // [PD] Correction: not quite portabel. SICStus does not have +l.
      //      SICStus has -l instead. Sigh.
      /* if QUINTUS
      String command = prolog + " +l pbtest_run";
      /* endif QUINTUS */
      /* if SICSTUS */
      String command = prolog + " -l pbtest_run";
      /* endif SICSTUS */
      System.err.println("DBG: Launching SICStus with \"" + command + "\"");
      // Load and start the Prolog example
      process = Runtime.getRuntime().exec(command);

      // Write all the error output that has no % in the start of the line
      BufferedReader err =
	new BufferedReader(new InputStreamReader(process.getErrorStream()));
      String line;
      while((line = err.readLine()) != null) {
	if (line.length() > 0 && line.charAt(0) != '%') {
	  System.err.println(line);

	  // When port is found, set it and notify that SICStus is running!
	  if (line.startsWith("port:")) {
	    port = Integer.parseInt(line.substring(5)); // e.g, port:4711
	    synchronized(this) {
	      notify();
	    }
	  }
	}
      }
    } catch (Exception e) {
      e.printStackTrace();
      port = -2;
    } finally {
      synchronized(this) {
	isShutdown = true;
	notify();
      }
    }
  }

  public synchronized int getPort(int timeout) throws InterruptedException {
    // Get the port from the running PBtest server (if not received within
    // 10 seconds, -1 will be returned.
    if (port == -1) {
      wait(timeout);
    }
    return port;
  }

  public synchronized boolean waitForShutdown(int timeout)
      throws InterruptedException {
    // Wait at most 10 seconds for the Prolog Server to shutdown
    if (!isShutdown) {
      wait(timeout);
    }
    return isShutdown;
  }

  public void shutdown() {
    process.destroy();
  }

  public static void main(String[] args) throws Exception {
    // Startup the prolog and show its err output!
    int test = 1;
    // [PD] 3.12.4 Timeout option added. The previous built-in
    //             default of 10 seconds was to short for blues.sics.se.
    int waiting_timeout =
	Integer.getInteger("se.sics.prologbeans.timeout", 0).intValue();
    PrologSession session = null;
    PBTest evalTest = new PBTest();
    try {
      Thread t = new Thread(evalTest);
      t.setDaemon(true);
      t.start();

      // Get the port from the SICStus process (and fail if port is an error value)
      int port = evalTest.getPort(waiting_timeout);
      if (port <= 0) {
	fail("could not start sicstus", test);
      }

      session = new PrologSession();
      session.setPort(port);
      if ((Integer.getInteger("se.sics.prologbeans.debugLevel", 0)).intValue() != 0) {
	  session.setTimeout(0);
      } else {
	  session.setTimeout(waiting_timeout);
      }
      session.connect();	// [PD] No autoconnect in PB4

      // Test 1. - evaluation!
      Bindings bindings = new Bindings().bind("E", "10+20.");
      QueryAnswer answer =
	session.executeQuery("evaluate(E,R)", bindings);
      PBTerm result = answer.getValue("R");
      if (result != null) {
	if (result.intValue() == 30) {
	  success("10+20=" + result, test++);
	} else {
	  fail("Execution failed: " + result, test);
	}
      } else {
	fail("Error " + answer.getError(), test);
      }

      // Test 2 - list reverse!
      bindings = new Bindings().bind("E", "reverse");
      answer = session.executeQuery("reverse(E,R)", bindings);
      result = answer.getValue("R");
      if (result != null) {
	if (listcompare(result,"esrever")) {
	  success("rev(reverse) -> " + result, test++);
	} else {
	  fail("Execution failed: " + result, test);
	}
      } else {
	fail("Error " + answer.getError(), test);
      }

      // Test 3 - show developers
      answer = session.executeQuery("devel\u00f6pers(Dev)");
      result = answer.getValue("Dev");
      if (result != null) {
	if (result.isProperList()) {
	  PBTerm list = result;
	  if (list.length() == 4 &&
	      "Joakim".equals(list.head().toString()) &&
	      "Niclas".equals(list.tail().head().toString()) &&
	      "Per".equals(list.tail().tail().head().toString()) &&
// [PD] 3.12.2 Do not use non-ASCII literals
//	      "åäöÅÄÖ".equals(list.getTermAt(4).toString())) {
	      "\u00e5\u00e4\u00f6\u00c5\u00c4\u00d6".equals(list.tail().tail().tail().head().toString())) {
	    success("develöpers -> " + result, test++);
	  } else {
	    fail("Execution failed: " + result, test);
	  }
	} else {
	  fail("Execution failed: " + result, test);
	}
      } else {
	fail("Error " + answer.getError(), test);
      }

      // Test 4 - send and receive a complex string-list
      String str = "foo\u1267bar";
      bindings = new Bindings().bind("L1", str);
      answer = session.executeQuery("send_receive(L1,L2)", bindings);
      result = answer.getValue("L2");
      if( result != null) {
	  if (listcompare(result, str)) {
	      success("send_receive(" + str + ") -> " + result, test++);
	  } else {
	      fail("Execution failed: " + result, test);
	  }
      } else {
	  fail("Error " + answer.getError(), test);
      }

      // Test 5 - send and receive a very large string (SPRM 9918)
      int stringLength = 100000;
      StringBuffer sb = new StringBuffer(stringLength);
      for (int i = 0; i < stringLength; i++) {
	  sb.append('x');
      }
//      System.out.println("sb.length()==" + sb.length());
//      System.out.println("sb.capacity()==" + sb.capacity());
      String longStr = sb.toString();
      bindings = new Bindings().bind("L1", longStr);
      answer = session.executeQuery("send_receive(L1,L2)", bindings);
      result = answer.getValue("L2");
      if( result != null) {
	  if (result.getString().equals(longStr)) {
	      success("OK (" + stringLength + " characters)", test++);
	  } else {
	      fail("Execution failed: " + stringLength + " characters", test);
	  }
      } else {
	  fail("Error " + answer.getError() + ", " + stringLength
	       + " characters", test);
      }

      // Test 6. shutdown server...
      session.executeQuery("shutdown");
      if (!evalTest.waitForShutdown(waiting_timeout)) {
	fail("shutdown", test++);
      } else {
	success("shutdown", test++);
      }
    } catch (Throwable e) {
      if (error == 0) {
	e.printStackTrace();
	fail("Exception " + e.getMessage(), test);
      }
    } finally {
      if (session != null) {
	session.disconnect();
      }
      evalTest.shutdown();
      System.exit(error);
    }
  }

  private static boolean listcompare(PBTerm pbl, String str) {
      int i;
      for (i = 0; i < str.length() && pbl.length() != 0; i++) {
	  long c = pbl.getArgument(1).intValue();
	  if (c != str.charAt(i)) {
	      return false;
	  } else {
	      pbl = pbl.getArgument(2);
	  }
      }
      if (i < str.length() || pbl.length() != 0) {
	  return false;
      }
      return true;
  }

  private static void fail(String msg, int test) {
    System.err.println("Execution failed: " + msg + " for test " + test);
    error = 1;
    throw new RuntimeException("");
  }

  private static void success(String msg, int test) {
    System.out.println("Test " + test + " succeeded: " + msg);
  }

} // PBTest
