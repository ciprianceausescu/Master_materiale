// ************************************************************************
// * Filename:	   JavaServer.java
// * Author:	   Jesper Eskilson <jojo@sics.se>
// * Created:	   Tue Jun  8 20:25:36 1999
// * CVS:          $Id: JavaServer.java,v 1.2 2000/10/30 17:37:37 matsc Exp $
// * Copyright (c) 1999, SICS
// ************************************************************************

package se.sics.jasper;

import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.reflect.*;
import se.sics.jasper.*;

/**
 * <b>This code is unsupported and unfinished. It is provided "as is" for
 * demontration purposes only.</b> The supported interface between Java
 * and SICStus is the {@link se.sics.jasper.SICStus SICStus} class and related classes.<p>
 * @deprecated Only intended as an example
 *
 Java server thread. Opens a server-socket and accepts connections
 for calling Java methods, according to the Jasper protocol.
*/
public class JavaServer extends Thread
{
    PrologEngine engine;
    ServerSocket sock;
    BufferedReader fromProlog = null;
    BufferedWriter toProlog = null;

    boolean keepRunning = true;

    static public void main(String argv[]) {
    }


    public JavaServer(PrologEngine engine)
	throws IOException, PrologException, SocketException
    {
	super("Java Server Thread");
	this.engine = engine;

	sock = new ServerSocket(0);
	sock.setSoTimeout(1000);

	engine.query("register_java([pid(0),streams([],[],[]),command([]),machine('" + 
		     engine.localHost + "'," + sock.getLocalPort() + ").");
    }

    void callMethod(Socket clientSock)
	throws IOException
    {
	String className,methodName;
	String type,value;
	int numArgs;
	Class cls;
	Class parameterTypes[];
	Method method;
	String objRef;
	boolean staticMethod;
	Object object = null, retVal;
	Object args[];
    
	// System.out.println("Inside callMethod()");
    
	try 
	    {
		objRef = PrologEngine.getNextLine(fromProlog);
		if (objRef.equals("[]")) {
		    staticMethod = true;
		    object = null;
		} else {
		    staticMethod = false;
		}

		className = PrologEngine.getNextLine(fromProlog);
		methodName = PrologEngine.getNextLine(fromProlog);
		numArgs = (Integer.valueOf(PrologEngine.getNextLine(fromProlog))).intValue();
	
		//System.out.println("class = " + className);
		//System.out.println("methodName = " + methodName);
	
		cls = Class.forName(className);
		parameterTypes = new Class[numArgs];
		args = new Object[numArgs];
	
		for (int i = 0; i < numArgs; i++) 
		    {
			type = PrologEngine.getNextLine(fromProlog);
			if (type.equals("end call")) {
			    System.out.println("Warning! premature end of argument list!");
			    break;
			}
	    
			className = PrologEngine.getNextLine(fromProlog);
			value = PrologEngine.getNextLine(fromProlog);
	    
			if (type.equals("primitive")) {
				// This ought to be optimizable in some way.
			    if (className.equals("int")) {
				parameterTypes[i] = java.lang.Integer.TYPE;
				args[i] = new Integer(value);
			    } else if (className.equals("byte")) {
				parameterTypes[i] = java.lang.Byte.TYPE;
				args[i] = new Byte(value);
			    } else if (className.equals("short")) {  
				parameterTypes[i] = java.lang.Short.TYPE;
				args[i] = new Short(value);
			    } else if (className.equals("char")) {  
				parameterTypes[i] = java.lang.Character.TYPE;
				if (value.length() > 0)
				    args[i] = new Character(value.charAt(0));
				else
				    args[i] = new Character((char)0);
			    } else if (className.equals("double")) {
				parameterTypes[i] = java.lang.Double.TYPE;
				args[i] = new Double(value);
			    } else if (className.equals("float")) {
				parameterTypes[i] = java.lang.Float.TYPE;
				args[i] = new Float(value);
			    } else if (className.equals("boolean")) { 
				parameterTypes[i] = java.lang.Boolean.TYPE;
				args[i] = new Boolean(value);
			    } else if (className.equals("long")) {
				parameterTypes[i] = java.lang.Long.TYPE;
				args[i] = new Long(value);
			    } 
			} else {
			    parameterTypes[i] = Class.forName(className);
			    args[i] = engine.objectStore.retrieveObject(new Long(value).longValue());
			    if (args[i] == null) {
				System.out.println("Illegal object reference index.");
				// Should throw exception!
				args[i] = null;
			    }

			    
			}
			
			System.out.println("Found class " + parameterTypes[i]);
		    }
		
		
		method = cls.getMethod(methodName,parameterTypes);
		
		System.out.println("Located method " + method);
		System.out.println("Invoking method");
		
		retVal = method.invoke(object,args);
	
		System.out.println("Return value = " + retVal);
	
		StringBuffer buf = new StringBuffer("returned(");
		Object array[];

		/* If the returned value is an array, convert it to a Prolog list. */
		if (retVal.getClass().isArray()) {
		    array = (Object [])retVal;
		    
		    buf.append("'$array',[");
		    
		    for (int i = 0; i < array.length - 1; i++) {
			buf.append(array[i].toString());
			buf.append(",");
		    }
		    buf.append(array[array.length-1].toString());
		    buf.append("]");
		    
		} else {
		    String cname = retVal.getClass().getName();
		    
		    // Booleans and numbers do not need quoting in Prolog.
		    if (retVal instanceof java.lang.Number ||
			retVal instanceof java.lang.Boolean)
			buf.append("'" + cname + "'," + retVal);
		    else
			buf.append("'" + cname + "','" + retVal + "'");
		}
		
		buf.append(").");
		PrologEngine.writeLine(toProlog,buf.toString());
	    }
	// These exceptions need to be passed on to Prolog in some way.
	catch (ClassNotFoundException cnfe) 
	    { cnfe.printStackTrace(); }
	catch (NoSuchMethodException nsme) 
	    { nsme.printStackTrace(); }
	catch (InvocationTargetException ite) 
	    { ite.printStackTrace(); }
	catch (IllegalAccessException iae) 
	    { iae.printStackTrace(); }
    
	clientSock.close();
    
    }

    public void run()
    {
	int faults = 0;

	// System.out.println("Callback handler started.");

	// Avoid infinite loop due to exceptions
	while (faults < 5 && keepRunning) {
	    try {
		// System.out.println("Callback handler waiting for callbacks...");
		Socket clientSock = sock.accept();
		String com;
		
		fromProlog = new 
		    BufferedReader(new InputStreamReader(clientSock.getInputStream()));
		toProlog = new 
		    BufferedWriter(new OutputStreamWriter(clientSock.getOutputStream()));
		
		com = PrologEngine.getNextLine(fromProlog);
		
		if (com != null) {
		    if (com.equals("begin call"))
			callMethod(clientSock);
		    else
			System.out.println("Unknown client command: " + com);
		} else {
		    System.out.println("getNextLine() returned null");
		}
	  
		clientSock.close();
		
		
	    } catch (InterruptedIOException ie) { 
	    } catch (IOException ioe) {
		ioe.printStackTrace();
		faults++;
	    }
	}
    }
}

