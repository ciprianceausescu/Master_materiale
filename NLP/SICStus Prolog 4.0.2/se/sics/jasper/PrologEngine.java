// ************************************************************************
// * Filename:	   PrologEngine.java
// * Author:	   Jesper Eskilson <jojo@sics.se>
// * Created:	   Wed May 12 12:02:07 1999
// * CVS:          $Id: PrologEngine.java,v 1.2 2000/10/30 17:37:38 matsc Exp $
// * Copyright (c) 1999, SICS
// ************************************************************************

package se.sics.jasper;

import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.reflect.*;
import se.sics.jasper.*;

class ObjectStoreWrapper 
{
    Object obj;
    long id;

    public ObjectStoreWrapper(Object obj, long id) 
    {
	this.obj = obj;
	this.id = id;
    }
    
}

/** 
 * Class to hold object references passed to Prolog. Only
 * used internally by PrologEngine.
 *
 * @author    $Author: matsc $
 * @version   $Revision: 1.2 $
 */
class ObjectStore 
{
    long id;
    Vector objs;

    public ObjectStore() 
    {
	id = 0;
	objs = new Vector(100,100);
    }

    long storeObject(Object obj) 
    {
	ObjectStoreWrapper objwrap;

	for (Enumeration e = objs.elements(); e.hasMoreElements() ;) {
	    objwrap = (ObjectStoreWrapper)e.nextElement();
	    if (objwrap.obj == obj)
		return objwrap.id;
	}
	
	objwrap = new ObjectStoreWrapper(obj,id++);
	objs.addElement(objwrap);

	return objwrap.id;
    }

    Object retrieveObject(long oid) {

	ObjectStoreWrapper objwrap;

	for (Enumeration e = objs.elements(); e.hasMoreElements() ;) {
	    objwrap = (ObjectStoreWrapper)e.nextElement();
	    if (objwrap.id == oid)
		return objwrap.obj;
	}
	
	return null;
    }
}


/** 
 * <b>This code is unsupported and unfinished. It is provided "as is" for
 * demontration purposes only.</b> The supported interface between Java
 * and SICStus is the {@link se.sics.jasper.SICStus SICStus} class and related classes.<p>
 * @deprecated Only intended as an example
 * Objects of this class represent a Prolog engine running in a
 * separate process. 
 * @see se.sics.jasper.SICStus
 * 
 * @author          $Author: matsc $
 * @version         $Revision: 1.2 $
 */
public class PrologEngine
{
    String prologMachine;
    int prologPortNum;
    Socket sock;
    BufferedReader fromProlog = null;
    BufferedWriter toProlog = null;
    Process prologProcess = null; // Used only when Java forks the Prolog process
				  // itself.
    BufferedWriter stdin;		// The standard streams of prologProcess.
    BufferedReader stdout, stderr;
    JavaServer jserver;
    Socket cbSock;
    String localHost = null;

    ObjectStore objectStore;

    static public int cbTestMethod(int x, double y)
    {
	System.out.println("cbTestMethod called. x = " + x + ", y = " + y);
	return (int)(x * y + 42);
    }

    static public Object cbTestMethod(Integer obj, int x, double y)
    {
	System.out.println("cbTestMethod called. obj = " + obj + ", x = " + x + ", y = " + y);
	return obj;
    }

    /**
     * Returns next non-empty line from 'reader' which does not start
     * with '#'.  Returns null on EOF.  
     */
    static public String getNextLine(BufferedReader reader)
	throws IOException
    {
	if (reader == null) return null;

	String s;
	do {      
	    Thread.yield();
	    s = reader.readLine();
	    // System.out.println("Read from " + reader + ": " + s);
	} while (s != null && (s.length() == 0 || s.charAt(0) == '#'));

	return s;
    }

    /**
     * Writes a string to a BufferedWriter, adds a newline and flushes
     * the stream 
     */
    static public void writeLine(BufferedWriter writer, String str) 
	throws IOException
    {
	Thread.yield();
	// System.out.println("wrote " + str + " to " + writer);
	writer.write(str, 0, str.length());
	writer.newLine();
	writer.flush();
    }

    protected void finalize()
	throws IOException, InterruptedException
    {
	// Halt the Prolog engine
	halt();
    }

    static void consumeOutput(BufferedReader reader)
	throws IOException
    {
	String s;
	System.out.println("consuming output from " + reader);
	while ((s = getNextLine(reader)) != null) {
	    Thread.yield();
	    System.out.println("> " + s);
	}
    }

    void initObjectStore()
    {
	objectStore = new ObjectStore();
    }

    void initJavaServer()
	throws IOException, ConnectionFailedException, PrologException
    {
	(jserver = new JavaServer(this)).start();    

	Thread.yield();
    }
  
    void findLocalHost() 
    {
	try {
	    localHost = InetAddress.getLocalHost().getHostAddress();
	} catch (Exception e) {
	    localHost = "127.0.0.1";
	}      
    }

    void shakeHands(BufferedWriter toProlog, BufferedReader fromProlog) 
	throws ConnectionFailedException, IOException
    {
	String s;

	s = getNextLine(fromProlog);
	
	if (!(s != null && s.equals("prologserver/magic1007")))
	    throw new ConnectionFailedException("expected magic number, got '" + s.trim() + "'");
	
	prologMachine = getNextLine(fromProlog);
	try {
	    prologPortNum = new Integer(s = getNextLine(fromProlog)).intValue();
	} catch (NumberFormatException nfe) {
	    throw new ConnectionFailedException(prologMachine+":"+prologPortNum+" gave a invalid integer (" + s + ") as port number");
	}
    }

    ////////////////////////////////////////////////////////////
    // 
    // Constructors
    //
    ////////////////////////////////////////////////////////////

    /** Starts a new Prolog process on the local machine */
    public PrologEngine(String cmdarray[])
	throws IOException, ConnectionFailedException, PrologException
    {
	String s;

	if (cmdarray.length == 0)
	    throw new ConnectionFailedException("empty Prolog command specified");

	findLocalHost();
	initObjectStore();

	prologProcess = Runtime.getRuntime().exec(cmdarray);
	stdin = new BufferedWriter(new OutputStreamWriter(prologProcess.getOutputStream()));
	stdout = new BufferedReader(new InputStreamReader(prologProcess.getInputStream()));
	stderr = new BufferedReader(new InputStreamReader(prologProcess.getErrorStream()));

	writeLine(stdin,"use_module(library(jserver)),start_slave.\n");

	// Confirm that this really is a Prolog server we're talking with.
	shakeHands(stdin,stdout);

	sock = new Socket(prologMachine,prologPortNum);
	fromProlog = new BufferedReader(new InputStreamReader(sock.getInputStream()));
	toProlog = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));

	initJavaServer();
    }

    /** Connect to a running Prolog process on 'machine:portNum'. */
    public PrologEngine(String machine, int portNum)
	throws IOException, ConnectionFailedException, PrologException
    {
	String s;

	findLocalHost();
	initObjectStore();

	sock = new Socket(machine, portNum);
	fromProlog = new BufferedReader(new InputStreamReader(sock.getInputStream()));
	toProlog = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));

	shakeHands(toProlog,fromProlog);

	initJavaServer();
    }
  
    public void halt()
	throws IOException, InterruptedException
    {
	jserver.keepRunning = false;
	sock.close();
	if (prologProcess != null) {
	    writeLine(stdin,"halt.");
	    prologProcess.waitFor();
	}
	jserver.join();
    }
    
    /** Ask an asynchronous query to the Prolog engine.
	public void queryAsynch()
	{
	}
    */
  
    /** Ask a query to the Prolog engine */
    public boolean query(String query)
	throws IOException, PrologException
    {
	String response;
	boolean result;
	/* TODO: analyse query for obvious syntax errors. */
    
	Thread.yield();

	String com = new String("metacall(" + query + ").");

	writeLine(toProlog,com);
	response = getNextLine(fromProlog);

	if (response.equals("yes")) {
	    String goal = getNextLine(fromProlog);
	    String var = null, val = null;
	    result = true;
	    
	    System.out.println("query '" + com + "' succeeded");
	    
	    do {
		Thread.yield();
		
		var = getNextLine(fromProlog);
		if (var.equals("eov"))
		    break;
		else
		    val = getNextLine(fromProlog);
		
		System.out.println("Variable " + var + " got value " + val);
	    } while (true);
	} else if (response.equals("no")) {
	    System.out.println("query '" + com + "' failed.");
	    result = false;
	} else if (response.equals("exception")) {
	    String excp = getNextLine(fromProlog);
	    
	    throw new PrologException(excp);
	} else {
	    /* Should this cause an exception, or should we simply return false? */
	    System.out.println("Huh?! unknown response \"" + response + "\" from Prolog server.");
	    result = false;
	}
	
	return result;
    }
    
    /** 
     * Ask a Prolog query given an array of Java objects. This method
     * is used if you need to send (references to) Java objects to 
     * Prolog.
     */
    public boolean query(Object querySpec[])
	throws IOException, PrologException
    {
	StringBuffer strQuery = new StringBuffer("");

	for (int i = 0; i < querySpec.length; i++ ) {
     
	    if (querySpec[i] instanceof java.lang.String) {
		strQuery.append(querySpec[i]);
	    } else {
		long oid;
		String className;

		// If the previous element in the array also was a non-string,
		// add a comma. This just for convenience, so we can write
		// 'query("foo(",obj1,obj2,obj3,")")' and get the query string
		// "foo(<obj1>,<obj2>,<obj3>
		if (i - 1 >= 0 && !(querySpec[i-1] instanceof java.lang.String)) {
		    strQuery.append(",");
		}

		oid = objectStore.storeObject(querySpec[i]);
		className = querySpec[i].getClass().getName();
		strQuery.append("'$object'('" + className + "',"+oid+")");
	    }
	}
	// System.out.println(strQuery);

	return query(strQuery.toString());
    }

    /* Syntactic sugar stuff */

    /** Instruct the Prolog engine to consult a file. This will cause
	the Prolog engine to perform the query "['<filename>']." */
    public boolean consult(String filename)
	throws IOException, PrologException
    {
	return query("['" + filename + "']");
    }

    /** Instruct the Prolog engine to compile a file. This will cause
	the Prolog engine to perform the query "compile('<filename>')." */
    public boolean compile(String filename)
	throws IOException, PrologException
    {
	return query("compile('" + filename + "')");
    }

    /** Instruct the Prolog engine to restore a saved state. This will cause
	the Prolog engine to perform the query "restore('<filename>')." */
    public boolean restore(String filename)
	throws IOException, PrologException
    {
	return query("restore('" + filename + "')");
    }

    static void example1(String argv[])
	throws IOException, PrologException, ConnectionFailedException
    {
	String hostAddr = argv[0];
	int portNum = new Integer(argv[1]).intValue();
    
	System.out.println("Connecting to " + hostAddr + ", port " + portNum);
	PrologEngine engine = new PrologEngine(hostAddr,portNum);
	System.out.println("Connected to Prolog system on " + 
			   engine.prologMachine + ":" + 
			   engine.prologPortNum);
    
	engine.consult("~/bench.pl");
	engine.query("bench(10000)");
    }

    static void example2(String argv[])
	throws IOException, PrologException, ConnectionFailedException
    {
	String hostAddr = argv[0];
	int portNum = new Integer(argv[1]).intValue();
    
	System.out.println("Connecting to " + hostAddr + ", port " + portNum);
	PrologEngine engine = new PrologEngine(hostAddr,portNum);
	System.out.println("Connected to Prolog system on " + 
			   engine.prologMachine + ":" + 
			   engine.prologPortNum);
    
	engine.query("(write(foo),nl)");
	engine.query("(X=foo(bar),(Y=bar(X,foo(X))),write(X),nl)");
	engine.query("(write('failed query'),nl,foo=bar)");
    }
  
    /*
    static void example3(String argv[])
	throws IOException, PrologException, ConnectionFailedException
    {
	PrologAtom atom = new PrologAtom("foobar");
	ObjectOutputStream str = new ObjectOutputStream(new FileOutputStream("foobar.txt"));
	str.writeObject(atom);
	str.flush();
    }
    */

    static void example4(String argv[])
	throws IOException, PrologException, ConnectionFailedException
    {
	System.out.println("Spawning slave server.");
	PrologEngine engine = new PrologEngine(new String[] {"/home/jojo/sicstus38"});
	System.out.println("Connected to Prolog system on " + 
			   engine.prologMachine + ":" + 
			   engine.prologPortNum);
     
	engine.query("(write(foo),nl)");
	engine.query("(X=foo(bar),(Y=bar(X,foo(X))),write(X),nl)");
	engine.query("(write('failed query'),nl,foo=bar)");   
    }    

    static void example5(String argv[])
	throws IOException, PrologException, ConnectionFailedException, InterruptedException
    {
	String hostAddr = argv[0];
	int portNum = new Integer(argv[1]).intValue();
    
	System.out.println("Connecting to " + hostAddr + ", port " + portNum);
	PrologEngine engine = new PrologEngine(hostAddr,portNum);
	System.out.println("Connected to Prolog system on " + 
			   engine.prologMachine + ":" + 
			   engine.prologPortNum);
    
	engine.query("use_module('/home/jojo/sicstus/sicstus3/se/sics/jasper/cbtest.pl')");
	engine.query("cbtest(X)");

	engine.halt();
    }
  
    static void example6(String argv[])
	throws IOException, PrologException, ConnectionFailedException, InterruptedException
    {
	String hostAddr = argv[0];
	int portNum = new Integer(argv[1]).intValue();
    
	System.out.println("Connecting to " + hostAddr + ", port " + portNum);
	PrologEngine engine = new PrologEngine(hostAddr,portNum);
	System.out.println("Connected to Prolog system on " + 
			   engine.prologMachine + ":" + 
			   engine.prologPortNum);

	engine.compile("/home1/jojo/sicstus3/se/sics/jasper/cbtest.pl");
	engine.query(new Object[] { "cbtest(", new Integer(42), ",X)" }); 

	engine.halt();
    }
  
    public static void main(String argv[])
    {
	try {
	    example6(argv);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}









