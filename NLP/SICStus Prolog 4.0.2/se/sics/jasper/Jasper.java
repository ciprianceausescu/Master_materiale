/*
 * Copyright © 2002 SICS AB
 */

package se.sics.jasper;

import java.lang.reflect.Constructor;
import java.lang.Boolean;
import java.lang.Thread;
import java.lang.ThreadLocal;
import java.util.Iterator;
//import java.util.LinkedList;
import java.util.Map;
//import java.util.Set;
import java.util.Stack;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPQuery;
import se.sics.jasper.SPException;
import se.sics.jasper.SPTerm;
import se.sics.jasper.Server;
import se.sics.jasper.Prolog;
import se.sics.jasper.Query;
import se.sics.jasper.Term;;


/**
 * This class provides the multi threaded functionality of the Java-Prolog
 * interface. The User's Manual documents how to use this class.
 *
 * <p> The interfaces {@link se.sics.jasper.Prolog},
 * {@link se.sics.jasper.Query} and {@link se.sics.jasper.Term} declare
 * the methods available to the Java programmer for access to SICStus runtime.
 *
 */
public class Jasper
{
    // *** FIX? remove the counters when non-Beta.
    private Counter tokencounter;
    private Counter queuecounter;
    private JasperProlog jasperProlog;
    private PrologServer jasperServer;
 
// Move all native methods to class SICStus. They should never have been
// here in the first place ...
/*
    private native void spCallVoidMethodByName(Object obj, String methname,
					       Object[] args, String typesig,
					       boolean staticP);
    private native Object spCallObjectMethodByName(Object obj,
						   String methname,
						   Object[] args,
						   String types,
						   boolean staticP);
    private native boolean spCallBooleanMethodByName(Object obj,
						     String methname,
						     Object[] args,
						     String types,
						     boolean staticP);
    private native byte spCallByteMethodByName(Object obj, String methname,
					       Object[] args, String types,
					       boolean staticP);
    private native char spCallCharMethodByName(Object obj, String methname,
					       Object[] args, String types,
					       boolean staticP);
    private native short spCallShortMethodByName(Object obj, String methname,
						 Object[] args, String types,
						 boolean staticP);
    private native int spCallIntMethodByName(Object obj, String methname,
					     Object[] args, String types,
					     boolean staticP);
    private native long spCallLongMethodByName(Object obj, String methname,
					       Object[] args, String types,
					       boolean staticP);
    private native float spCallFloatMethodByName(Object obj, String methname,
						 Object[] args, String types,
						 boolean staticP);
    private native double spCallDoubleMethodByName(Object obj, String methname,
						   Object[] args, String types,
						   boolean staticP);
*/

    private static int jasperDebug = Integer.getInteger("se.sics.jasper.debugLevel", 0).intValue();

    final boolean debugging()
    {
	return debugging(1);
    }

    final boolean debugging(int level)
    {
	return jasperDebug >= level;
    }

    static void debugPrintln(String msg)
    {
	System.err.println("Thread " + Thread.currentThread().getName() + ": "
			   + msg);
	System.err.flush();
    }

    static void debugPrint(String msg)
    {
	System.err.print("Thread " + Thread.currentThread().getName() + ": "
			 + msg);
	System.err.flush();
    }

/*
    static void DebugPrint(String s)
    {
	DebugPrint(0, s);
    }
    static void DebugPrintln(String s)
    {
	DebugPrintln(0, s);
    }
    static void DebugPrint(int level, String s)
    {
	if (jasperDebug >= level) {
	    System.err.print(Thread.currentThread().getName() + ": ");
	    System.err.print(s);
	    System.err.flush();
	}
    }
    static void DebugPrintln(int level, String s)
    {
	if (jasperDebug >= level) {
	    System.err.print(Thread.currentThread().getName() + ": ");
	    System.err.println(s);
	    System.err.flush();
	}
    }
*/

    // *** FIX? remove the counters when non-Beta.
    //          Or at least remove most of the usage.
    class Counter
    {
	private int c = 0;
	public int next()
	{
	    return c += 1;
	}
    }

    class TokenQueue
    {
	private String name;
//L	private LinkedList qlist = new LinkedList();
	private Stack qstack = new Stack();
	synchronized public void put(Token t)
	{
	    if (debugging(5)) debugPrintln("Entering TokenQueue.put()");
 	    if (debugging(5)) debugPrintln("    this queue: " + this);
	    boolean wasEmpty = isEmpty();
	    if (debugging(5)) debugPrintln("    wasEmpty==" + wasEmpty);
	    if (debugging(5)) debugPrintln("    putting token: " + t);
	    t.debugPrintToken(10);
//L	    qlist.addFirst(t);
	    qstack.push(t);
//L 	    if (debugging(10)) debugPrintln("    queue size: " + qlist.size());
 	    if (debugging(10)) debugPrintln("    queue size: " + qstack.size());
	    if (wasEmpty) { notify(); }
	}
	synchronized public Token get() throws InterruptedException
	{ 
	    if (debugging(5)) debugPrintln("Entering TokenQueue.get()");
 	    if (debugging(5)) debugPrintln("    this queue: " + this);
	    waitfor();
	    if (debugging(5)) debugPrintln("in TokenQueue.get(), after waitfor()");
 	    if (debugging(5)) debugPrintln("    this queue:" + this);
//L 	    if (debugging(10)) debugPrintln("    queue size: " + qlist.size());
 	    if (debugging(10)) debugPrintln("    queue size: " + qstack.size());
//L	    Token tok = (Token)qlist.removeLast();
	    Token tok = (Token)qstack.pop();
	    if (debugging(5)) debugPrintln("    getting token: " + tok);
	    tok.debugPrintToken(10);
	    return tok; 
	}
	synchronized public boolean isEmpty()
	{ 
//L	    return qlist.isEmpty(); 
	    return qstack.empty(); 
	}
	synchronized public void waitfor() throws InterruptedException
	{
	    if (debugging(5)) debugPrintln("Entering TokenQueue.waitfor()");
	    if (debugging(5)) debugPrintln("    this queue: " + this);
	    if (debugging(5)) debugPrintln("    isEmpty():  " + isEmpty());
//	    if (isEmpty()) {
	    while (isEmpty()) {
		wait();
	    }
	    if (debugging(5)) debugPrintln("Exiting TokenQueue.waitfor()");
	}
	public String toString()
	{
	    return this.name;
	}
	TokenQueue(String name)
	{
	    // *** FIX? Remove the counters when non-Beta.
	    //          Use toString() here instead of the counter value?
	    //          Well, let's leave the counters for a while ...
	    this.name = "TokenQueue-" + queuecounter.next() + "-" + name;
	    if (debugging(5)) debugPrintln( "Creating TokenQueue(): " + this.name);
	}
    }

    class Token
    {
	private String name;
	private TokenQueue myqueue;     // Return adress.
	private Object obj;
	private String methodname;
	private String typesig;
	private Object[] args;
	private boolean staticP;      	// static or instance
	private boolean hasResult;
	private Object result;
	private Exception excp;

	boolean isStopServerMessage()   { return false; }
	TokenQueue getQueue()           { return myqueue; }
	Object    getObject()           { return obj; }
	String    getMethodname()       { return methodname; }
	String    getTypesig()          { return typesig; }
	char      getReturnType()
	{
	    return typesig.charAt(typesig.lastIndexOf(')') + 1);
	}
	Object[]  getArgs()             { return args; }
	boolean   getStaticP()          { return staticP; }
	void      setResult(Object res) { this.result = res; hasResult = true; }
	Object    getResult()           { return result; }
	void      setException(Exception ex) { this.excp = ex; }
	boolean   checkException()      { return (excp != null); }
	Exception getException()        { return excp; }
	boolean   checkResult()         { return hasResult; }

	public String toString()
	{
	    return this.name;
	}
	void debugPrintToken(int debugLevel)
	{
	    if (debugging(debugLevel)) {
		debugPrintln("  *Token*:");
		debugPrintln("    token:      " + name);
		debugPrintln("    myqueue:    " + myqueue);
		debugPrintln("    obj.class:  " + obj.getClass().getName());
		debugPrintln("    obj.hashCode: " + obj.hashCode());
		debugPrintln("    methodname: " + methodname);
		debugPrintln("    typesig:    " + typesig);
		debugPrintln("    staticP:    " + staticP);
		if (result == null) {
		    debugPrintln("    result is null");
		} else {
		    debugPrintln("    result is " + result.getClass().getName());
		}
		if (excp == null) {
		    debugPrintln("    excp is null");
		} else {
		    debugPrintln("    excp is not null");
		}
	    }
	}
	Token(String name, TokenQueue tq, Object obj, String methodname,
		  Object[] args, String typesig, boolean staticP)
	{
	    // *** FIX? Remove the counters when non-Beta.
	    //          Use toString() here instead of the counter value.
	    // or maybe not ...
	    this.name = "Token-" + tokencounter.next() + "-" + name;
	    this.myqueue = tq;
	    this.obj = obj;
	    this.methodname = methodname;
	    this.args = args;
	    this.typesig = typesig;
	    this.staticP = staticP;
	    this.result = null;
	    this.hasResult = false;
	    this.excp = null;
	}
    }

    class NewObjectToken extends Token
    {
	public char getReturnType() { return 'N'; }

	NewObjectToken(String name, TokenQueue tq, Object obj,
		       String methodname, Object[] args, String typesig)
	{
	    super(name, tq, obj, methodname, args, typesig, true);
	}
    }

    class StopToken extends Token
    {
	boolean isStopServerMessage()   { return true; }

	void debugPrintToken(int debugLevel)
	{
	    if (debugging(debugLevel)) debugPrintln("  *StopToken*");
	}

// 	StopToken(String name, TokenQueue tq)
// 	{
// 	    super(name, tq, null, null, null, null, false);
// 	}
	StopToken()
	{
	    super(null, null, null, null, null, null, false);
	}
    }

    class PrologServer implements Runnable, Server
    {
	private String bootPath;
	private String[] argv;
	private String savFile;
	private SICStus sp = null;
	private TokenQueue serverqueue = null;
	private TokenQueue clientqueue = null;
	private boolean runFlag = true;

	void initPrologServer()
	{
 	    serverqueue = new TokenQueue("Server");
	    clientqueue = new TokenQueue("Client");
	    runFlag = true;
	}
	PrologServer(String[] argv, String bootPath, String savFile)
	{
	    this.argv = argv;
	    this.bootPath = bootPath;
	    this.savFile = savFile;
	    sp = null;
	    initPrologServer();
	}
	PrologServer(SICStus sp)
	{
	    setSICStus(sp);
	    initPrologServer();
	}

	public void run()
	{
	    if (sp == null) {
		SICStus sp;
		try {
		    if (debugging(10)) debugPrintln("in PrologServer.run(): trying to create sp");
		    sp = new SICStus(argv, bootPath);
		    if (debugging(10)) debugPrintln("in PrologServer.run(): sp created");
		    if (savFile != null) { sp.restore(savFile); }
		} catch (Throwable th) {
		    setNoSICStus();
		    throw new Error("Cannot create a SICStus object. " + th);
		}
		setSICStus(sp);
	    }
	    if (!sp.spSetThreadServerMode(true)) {
		throw new Error("Cannot set thread server mode.");
	    }
	    TokenQueue srvq = getServerqueue();
	    try {
		while (runFlag) {
		    if (debugging(5)) debugPrintln("in PrologServer.run(); runFlag-loop");
 		    Token message = srvq.get();
		    boolean stop = message.isStopServerMessage();
 		    if (stop) break;
		    while (!message.checkResult()) {
			if (debugging(10)) debugPrintln("    message:");
			message.debugPrintToken(10);
			callMethod(sp, message);
//			clientqueue = message.getQueue();
			clientqueue.put(message);
			message = srvq.get();
			stop = message.isStopServerMessage();
			if (stop) break;
		    }
 		    if (stop) break;
		}
	    } catch (Exception ex) {
		if (debugging(10)) debugPrint("Exception in PrologServer.run():");
		ex.printStackTrace(System.err);
	    } finally {
		sp.spSetThreadServerMode(false);
	    }
	}

	public void stopServer()
	{
	    if (runFlag) {
		TokenQueue srvq = getServerqueue();
		Token stopToken = new StopToken();
		runFlag = false;
		srvq.put(stopToken);
	    }
	}

// [PD] 3.9 Does not have to be public?
//	synchronized public void setSICStus(SICStus sp)
	synchronized void setSICStus(SICStus sp)
	{
	    this.sp = sp;
	    sp.setServer(this);
	    notify();
	}
	synchronized private void setNoSICStus()
	{
	    notify();
	}
	synchronized public SICStus getSICStus() throws InterruptedException
	{
	    if (sp == null) {
		wait();
	    }
	    return sp;
	}
	TokenQueue getServerqueue() { return serverqueue; }
	TokenQueue getClientqueue() { return clientqueue; }

	// The callBack<Foo> methods are called from jasper.c
	// *NOTE*: They are therefore called in the server thread!
	// When they are called the PrologClient is waiting for
	// the PrologServer to service a request. The client
	// checks for callbacks, executes one or more callbacks
	// and then continues to wait for a notification from the
	// server.

	public Token callBack(Object obj, String methname,
			      Object[] args, String typesig, boolean staticP)
	    throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering PrologServer.callBack()");
	    Token cbtok = new Token("Server",serverqueue,obj,methname,args,typesig, staticP);
	    return callBackLoop(cbtok);
	    
	}

	Token callBackLoop(Token cbtok) throws InterruptedException, Exception
	{
	    clientqueue.put(cbtok);
	    cbtok = serverqueue.get();
	    while (!cbtok.checkResult()) {
		callMethod(sp, cbtok);
//		TokenQueue clientqueue = cbtok.getQueue();
		clientqueue.put(cbtok);
		cbtok = serverqueue.get();
	    }
// cbtok now has a return value, and possibly an exception value.
// Same as in PrologClient.sendMessage(): throw the exception. It will be
// caught by callMethod() and propagated further.
// [PD] 3.9.1. No, no! That will break things. We must not throw an exception
// here, since that will cause disrupt the calling sequence in the server
// thread. Or maybe not? Throwing the exception is the only way to propagate it,
// isn't it?
// Simply pass the token to the client, and let it handle the exception.
// *** FIX? We should perhaps not propagate InterruptedException?

	    if (cbtok.checkException()) {
		Exception ex = cbtok.getException();
		throw (Exception)ex.fillInStackTrace();
	    }

	    return cbtok;
	}

	public Object callBackNewObject(Object obj, String methname,
					Object[] args, String typesig)
	    throws InterruptedException, Exception
	{
	    Token cbtok = new NewObjectToken("Server",serverqueue,obj,methname,args,typesig);
	    Token cbtok2 = callBackLoop(cbtok);
 	    return cbtok2.getResult();
	}

	public void callBackVoid(Object obj, String methname,
				 Object[] args, String typesig,
				 boolean staticP)
	    throws InterruptedException, Exception
        {
	    if (debugging(5)) debugPrintln("Entering PrologServer.callBackVoid()");
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
	}

	public Object callBackObject(Object obj, String methname,
				     Object[] args, String typesig,
				     boolean staticP)
	    throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering PrologServer.callBackObject()");
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return cbtok.getResult();
	}

	public boolean callBackBoolean(Object obj, String methname,
				       Object[] args, String typesig,
				       boolean staticP)
	    throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering PrologServer.callbackBoolean()");
	    if (debugging(10)) debugPrintln("    obj:      " + obj);
	    if (debugging(10)) debugPrintln("    methname: " + methname);
	    if (debugging(10)) debugPrintln("    args:     " + args);
	    if (debugging(10)) debugPrintln("    typesig:  " + typesig);
	    if (debugging(10)) {
		debugPrintln("x"+methname+"y");
		if (methname.equals("barbool")) {
		     debugPrintln("["+methname+"]");
		     debugPrintln("    args[6]==" + args[6]);
		}
	    }
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Boolean)cbtok.getResult()).booleanValue();
	}

	public byte callBackByte(Object obj, String methname,
				 Object[] args, String typesig,
				 boolean staticP)
	    throws InterruptedException, Exception
	{
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Byte)cbtok.getResult()).byteValue();
	}

	public char callBackChar(Object obj, String methname,
				 Object[] args, String typesig,
				 boolean staticP)
	    throws InterruptedException, Exception
	{
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Character)cbtok.getResult()).charValue();
	}

	public short callBackShort(Object obj, String methname,
				   Object[] args, String typesig,
				   boolean staticP)
	    throws InterruptedException, Exception
	{
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Short)cbtok.getResult()).shortValue();
	}

	public int callBackInt(Object obj, String methname,
			       Object[] args, String typesig,
			       boolean staticP)
	    throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering PrologServer.callBackInt()");
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
	    if (debugging(10)) debugPrintln("in PrologServer.callBackInt(), after callBack()");
	    if (debugging(10)) debugPrintln("    cbtok.getResult()==" + cbtok.getResult());
	    if (debugging(10)) debugPrintln("    intvalue: " + ((Integer)cbtok.getResult()).intValue());
 	    return ((Integer)cbtok.getResult()).intValue();
	}

	public long callBackLong(Object obj, String methname,
				 Object[] args, String typesig,
				 boolean staticP)
	    throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering PrologServer.callbackLong()");
	    if (debugging(10)) debugPrintln("    obj:      " + obj);
	    if (debugging(10)) debugPrintln("    methname: " + methname);
	    if (debugging(10)) debugPrintln("    args:     " + args);
	    if (debugging(10)) debugPrintln("    typesig:  " + typesig);
	    if (debugging(10)) debugPrintln("    staticP:  " + staticP);
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
	    cbtok.debugPrintToken(2);
 	    return ((Long)cbtok.getResult()).longValue();
	}

	public float callBackFloat(Object obj, String methname,
				   Object[] args, String typesig,
				   boolean staticP)
	    throws InterruptedException, Exception
	{
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Float)cbtok.getResult()).floatValue();
	}

	public double callBackDouble(Object obj, String methname,
					 Object[] args, String typesig,
				     boolean staticP)
	    throws InterruptedException, Exception
	{
	    Token cbtok = callBack(obj, methname, args, typesig, staticP);
 	    return ((Double)cbtok.getResult()).doubleValue();
 	}

	// called by "glue" (i.e. spnative), in the server thread.
	JasperTerm newGlueJasperTerm(SPTerm spt)
	    throws InterruptedException, Exception
	{
	    if (debugging()) debugPrintln("Entering newGlueJasperTerm()");
//	    return new JasperTerm(this, spt);
	    if (debugging()) {
		Object obj0[] = new Object[] {null};
		debugPrintln("in newGlueJasperTerm() (1)");
		debugPrintln("    spt==" + spt);
		debugPrintln("    spt.hashCode()==" + spt.hashCode());
	    }	    
	    Object obj[] = new Object[] {spt};
	    if (debugging()) debugPrintln("in newGlueJasperTerm() (2)");
 	    return (JasperTerm)callBackObject(this,
 					      "newGlueJasperTermFromClient",
					      obj,
 					      "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
 					      false);
	}

	// Must be called from the client thread!
	JasperTerm newGlueJasperTermFromClient(SPTerm spt)
	    throws InterruptedException //, Exception
	{
	    return new JasperTerm(jasperProlog, this, spt);
	}
    }

    class PrologClient
    {
	PrologServer myServer;
	SICStus sp = null;
	private TokenQueue serverqueue = null;
//	private TokenQueue clientqueue = null;
	TokenQueue clientqueue = null;
	Thread initialThread;
	
	public Server getServer() { return myServer; }

	void pushCaller(Prolog caller)
	{
// [PD] 3.9.1 SPRM 3305
//	    ((Stack)(JasperGlobalCallerStack.get())).push(this);
	    ((Stack)(JasperGlobalCallerStack.get())).push(caller);
	}
	void popCaller()
	{
	    ((Stack)(JasperGlobalCallerStack.get())).pop();
	}

// [PD] 3.9.1 SPRM 3305
//	Token sendMessage(String typesig, Object obj, String methodName,
// [PD] 3.10.1 Make synchronized so multiple threads can use the same
//             PrologClient object.
	synchronized Token sendMessage(PrologClient owner,
			  String typesig, Object obj, String methodName,
			  Object[] args) throws Exception
	{
// [PD] 3.10.1 Allow multiple threads to use the same PrologClient object
// 	    if (debugging()) {
// 		Thread callingThread = Thread.currentThread();
// 		if (initialThread != callingThread) {
// 		    throw new Error("Illegal calling thread in PrologClient.sendMessage(); initialThread==" + initialThread + "; callingThread==" + callingThread);
// 		}
// 	    }
	    Token reply;
	    Token request = new Token(Thread.currentThread().getName(), clientqueue, obj, methodName, args, typesig, false);
	    serverqueue.put(request);
	    reply = clientqueue.get();
	    while (!reply.checkResult()) {
// [PD] Can we do this cast just like that? What if `this' is e.g. a
//	JasperTerm?
//      [PD, later] No, of course we can't. See SPRM 3305.
//		pushCaller((Prolog)this);
		if (debugging(5)) debugPrintln("In sendMessage(), owner==" + owner);
		pushCaller((Prolog)owner);
		callMethod(sp, reply);
		popCaller();
		serverqueue.put(reply);
		reply = clientqueue.get();
	    }
// *** FIX? We should perhaps not propagate InterruptedException?
	    if (debugging(5)) debugPrintln("In sendMessage(), checking for exception (this is expected)");
	    reply.debugPrintToken(5);
	    if (reply.checkException()) {
		Exception ex = reply.getException();
		throw (Exception)ex.fillInStackTrace();
	    }
	    return reply;
	}

// *** FIX? Consider making this private.
// [PD] 3.9.1 SPRM 3305
	Token sendMessage(String typesig, Object obj, String methodName,
			  Object[] args) throws Exception
	{
	    if (debugging(5)) debugPrintln("Entering sendMessage(), this.getClass()==" + this.getClass());
	    return sendMessage(this, typesig, obj, methodName, args);
	}


	private void initQueues(PrologServer server)
	{
	    this.myServer = server;
	    serverqueue = myServer.getServerqueue();
 	    clientqueue = myServer.getClientqueue();
	}
// 	private void initQueues(PrologServer server, TokenQueue clientq)
// 	{
// 	    this.myServer = server;
// 	    serverqueue = myServer.getServerqueue();
//  	    this.clientqueue = clientq;
// 	}
	PrologClient(PrologServer server)
	{
	    if (debugging()) debugPrintln("Creating a PrologClient from thread " + Thread.currentThread());
	    initQueues(server);
	    initialThread = Thread.currentThread();
	}
	PrologClient(PrologServer server, SICStus sp)
	{
/*	    initQueues(server);
	    initialThread = Thread.currentThread();
*/
	    this(server);
	    this.sp = sp;
	}
// 	PrologClient(PrologServer server, SICStus sp, TokenQueue clientqueue)
// 	{
// 	    initQueues(server, clientqueue);
// 	    this.sp = sp;
// 	}
    }

    class JasperProlog extends PrologClient implements Prolog
    {
	public Query openPrologQuery(String string, Map varMap)
	    throws Exception
	{
	    if (debugging(5)) debugPrintln("entering JasperProlog.openPrologQuery()");
// [PD] 3.9.1 SPRM 3305
//	    return new JasperQuery(myServer, string, varMap);
	    return new JasperQuery(this, myServer, string, varMap);
	}

	private boolean queryHelper(String methodName, String string,
				    Map varMap)
	    throws Exception
	{
	    if (debugging(5)) debugPrintln("entering JasperProlog.queryHelper()");
	    if (debugging(10)) debugPrintln("    string==" + string);
	    if (varMap != null) {
		// Unwrap all SPTerms
		Iterator it = varMap.entrySet().iterator();
		while (it.hasNext()) {
		    Map.Entry me = (Map.Entry)it.next();
		    Object val = me.getValue();
		    if (debugging(10)) debugPrintln( "    val.getClass()==" + val.getClass());
		    if (val instanceof JasperTerm) {
			me.setValue(((JasperTerm)val).getSPTerm());
		    }
		}
	    if (debugging(10)) debugPrintln("JasperProlog.queryHelper(), after unwrap");
	    }
	    Token request =
		this.sendMessage("(Ljava/lang/String;Ljava/util/Map;)Z",
				 sp, methodName,
				 new Object[] {string, varMap});
	    if (varMap != null) {
		// Wrap all SPTerms in JasperTerm objects
		Iterator it = varMap.entrySet().iterator();
		while (it.hasNext()) {
		    Map.Entry me = (Map.Entry)it.next();
		    me.setValue(new JasperTerm(this, myServer, (SPTerm)me.getValue()));
		}
	    }
	    if (debugging(10)) debugPrintln("JasperProlog.queryHelper(), after wrap");
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean query(String string, Map varMap)
	    throws Exception
	{
	    if (debugging(5)) debugPrintln("entering JasperProlog.prologQuery()");
	    return queryHelper("query", string, varMap);
	}

	public boolean queryCutFail(String string, Map varMap)
	    throws Exception
	{
	    if (debugging(5)) debugPrintln("entering JasperProlog.prologQueryCutFail()");
	    return queryHelper("queryCutFail", string, varMap);
	}

	public Term newTerm() throws InterruptedException, Exception
	{
	    if (debugging(5)) debugPrintln("Entering JasperProlog.newTerm()");
	    Token request = this.sendMessage("()Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {});
	    SPTerm spt = (SPTerm)request.getResult();
// [PD] 3.11.0 SPTerm.toString() cannot be called from the client thread
//	    if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt==" + spt.toString());
	    if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt==" + spt.superToString());
	    if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt.isValid()==" + spt.isValid());
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(Term t) throws InterruptedException, Exception
	{
	    Token request = this.sendMessage("(Lse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {((JasperTerm)t).getSPTerm()});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(int i) throws InterruptedException, Exception // integer
	{
	    Token request = this.sendMessage("(I)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {new Integer(i)});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(long j) throws InterruptedException, Exception // integer
	{
	    Token request = this.sendMessage("(J)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {new Long(j)});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(double d) throws InterruptedException, Exception // float
	{
	    Token request = this.sendMessage("(D)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {new Double(d)});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(float f) throws InterruptedException, Exception // float
	{
	    Token request = this.sendMessage("(F)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {new Float(f)});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(String a) throws InterruptedException, Exception // atom
	{
	    Token request = this.sendMessage("(Ljava/lang/String;)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {a});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newTerm(String functor, Term args[]) throws InterruptedException, Exception // functor
	{
	    if (debugging(5)) debugPrintln("Entering JasperProlog.newTerm(String,Term[])");
	    // *** FIX? Is `new SPTerm[]' OK?
	    SPTerm[] sptargs = new SPTerm[args.length];
	    if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), sptargs created");
	    for (int i = 0; i < args.length; i++) {
		if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), i==" + i);
		sptargs[i] = ((JasperTerm)args[i]).getSPTerm();
	    }
	    if (debugging(10)) debugPrintln("in JasperProlog.newTerm(String,Term[]), before sendMessage");
	    Token request = this.sendMessage("(Ljava/lang/String;[Lse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {functor, sptargs});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	

	public Term prologReadFromString(String string, java.util.Map varMap) throws InterruptedException, Exception
	{
	    Token request = sendMessage("(Ljava/lang/String;Ljava/util/Map;)Lse/sics/jasper/Term;",
					sp, "prologReadFromString",
					new Object[] {string, varMap});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term newVariable() throws InterruptedException, Exception
	{
	    Token request = sendMessage("()Lse/sics/jasper/Term;",
					sp, "newVariable",
					new Object[] {});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term consFunctor(String functor, Term[] args) throws InterruptedException, Exception
	{
	    SPTerm[] sptargs = new SPTerm[args.length];
	    for (int i = 0; i < args.length; i ++) {
		sptargs[i] = ((JasperTerm)args[i]).getSPTerm();
	    }
	    Token request = sendMessage("(Ljava/lang/String;[Lse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					sp, "consFunctor",
					new Object[] {functor, sptargs});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}
	public Term consList(Term head, Term tail) throws InterruptedException, Exception
	{
	    SPTerm spthead = ((JasperTerm)head).getSPTerm();
	    SPTerm spttail = ((JasperTerm)tail).getSPTerm();
	    Token request = sendMessage("(Lse/sics/jasper/Term;Lse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					sp, "consList",
					new Object[] {spthead, spttail});
	    SPTerm spt = (SPTerm)request.getResult();
	    return new JasperTerm(this, myServer, spt);
	}

	public Term newObjectTerm(Object obj) // [PD] 3.9.2 SPRM 3141
	    throws InterruptedException, ConversionFailedException, Exception
	{
	    Token request = this.sendMessage("()Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {});
	    SPTerm spt = (SPTerm)request.getResult();
	    // [PD] 3.10.2 We can't call putObject directly since we are not
	    // in the server thread.
	    //spt.putObject(obj);
	    this.sendMessage("(Ljava/lang/Object;)Lse/sics/jasper/SPTerm;",
			     spt, "putObject",
			     new Object[] {obj});
	    return new JasperTerm(this, myServer, spt);
	}

	public Term numberFromString(String str) // [PD] 3.9.2 SPRM 3141
	    throws InterruptedException, ConversionFailedException, Exception
	{
	    Token request = this.sendMessage("()Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {});
	    SPTerm spt = (SPTerm)request.getResult();
	    spt.putNumberChars(str);
	    return new JasperTerm(this, myServer, spt);
	}

	public Term listFromString(String str) // [PD] 3.9.2 SPRM 3141
	    throws InterruptedException, ConversionFailedException, Exception
	{
	    Token request = this.sendMessage("()Lse/sics/jasper/Term;",
					     sp, "newTerm",
					     new Object[] {});
	    SPTerm spt = (SPTerm)request.getResult();
	    spt.putListChars(str);
	    return new JasperTerm(this, myServer, spt);
	}

	void initProlog(PrologServer server) throws InterruptedException

	{
	    this.sp = server.getSICStus();
	    if (this.sp == null) {
		throw new Error("Can't get SICStus from server");
	    }
	    if (debugging(5)) debugPrint("In JasperProlog.initProlog(); clientqueue==" + clientqueue);
	}

	JasperProlog(PrologServer server) throws InterruptedException
	{
	    super(server);
	    if (debugging(5)) debugPrintln( "Creating a JasperProlog");
	}
	JasperProlog(PrologServer server, SICStus sp)
	    throws InterruptedException
	{
	    super(server, sp);
	    if (debugging(5)) debugPrintln( "Creating a JasperProlog");
	}
    }


// [PD] 3.10.2
//    class JasperQuery extends PrologClient implements Query
    class JasperQuery implements Query
    {
	SPQuery spq;
	Map myMap;
	JasperProlog owner;	// [PD] 3.9.1 SPRM 3305

	public void close() throws Exception
	{
// [PD] 3.9.2 SPRM 3647
//	    Token dummy = this.sendMessage("()V", spq, "close", new Object[]{});
	    Token dummy = owner.sendMessage(owner, "()V", spq, "close",
					   new Object[]{});
	}

	public void cut() throws Exception
	{
// [PD] 3.9.2 SPRM 3647
//	    Token dummy = this.sendMessage("()V", spq, "cut", new Object[] {});
	    Token dummy = owner.sendMessage(owner, "()V", spq, "cut",
					   new Object[] {});
	}

	public boolean nextSolution()
	    throws Exception
	{
// [PD] 3.9.1 SPRM 3305
//	    Token request = this.sendMessage("()Z", spq, "nextSolution",
	    Token request = owner.sendMessage(owner, "()Z", spq, "nextSolution",
					     new Object[]{});
	    boolean result = ((Boolean)request.getResult()).booleanValue();
	    if (result && myMap != null) {
		// Wrap all SPTerms in JasperTerm objects
		Iterator it = myMap.entrySet().iterator();
		while (it.hasNext()) {
		    Map.Entry me = (Map.Entry)it.next();
		    Object val = me.getValue();
		    if (val instanceof SPTerm) {
			me.setValue(new JasperTerm(owner, jasperServer, (SPTerm)val));
		    }
		}
	    }
	    return result;
	}

// [PD] 3.9.1 SPRM 3305
//	JasperQuery(PrologServer server, String string, Map varMap)
	JasperQuery(JasperProlog owner, PrologServer server, String string,
		    Map varMap) throws Exception
	{
// [PD] 3.10.2
//	    super(server, server.getSICStus());
	    if (debugging(5)) debugPrintln( "Creating a JasperQuery");
	    this.owner = owner;	// [PD] 3.9.1 SPRM 3305
	    myMap = varMap;
	    if (varMap != null) {
		// Unwrap all SPTerms
		Iterator it = varMap.entrySet().iterator();
		while (it.hasNext()) {
		    Map.Entry me = (Map.Entry)it.next();
		    Object val = me.getValue();
		    if (val instanceof JasperTerm) {
			me.setValue(((JasperTerm)val).getSPTerm());
		    }
		}
	    }
	    Token request = owner.sendMessage("(Ljava/lang/String;Ljava/util/Map;)Lse/sics/jasper/SPQuery;",
					     owner.sp, "openQuery",
					     new Object[] {string, varMap});
	    spq = (SPQuery)request.getResult();
	}
    }


/* [PD] 3.9 Should not have to be public. */
//    public class JasperTerm extends PrologClient implements Term
    class JasperTerm implements Term
    {
	private SPTerm spt;
	SPTerm getSPTerm() { return spt; }
	JasperProlog owner;	// [PD] 3.10.2

	// [PD] 3.9.1 Used by "glue code" (jasper.c). Must be called in the
	//            server thread.
	long GetNativeTermRef() throws IllegalTermException
	{
	    return getSPTerm().GetNativeTermRef();
	}

	public int compare(Term with) throws IllegalTermException, Exception
	{
	    SPTerm sptwith = ((JasperTerm)with).getSPTerm();
	    Token request = owner.sendMessage("(Lse/sics/jasper/Term;)I",
					      spt, "compare",
					      new Object[] {sptwith});
	    return ((Integer)request.getResult()).intValue();
	}
	public void delete() throws Exception
	{
	    Token request = owner.sendMessage("()V", spt, "delete",
					      new Object[] {});
	}
	public Term getArg(int i, Term arg) throws InterruptedException, Exception
	{
	    SPTerm sptarg = ((JasperTerm)arg).getSPTerm();
	    Token request = owner.sendMessage("(ILse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					spt, "getArg",
					new Object[] {new Integer(i), sptarg});
	    return this;
	}
	public double getDouble() throws ConversionFailedException, IllegalTermException, Exception
	{
	    Token request = owner.sendMessage("()D", spt, "getDouble",
					      new Object[] {});
	    return ((Double)request.getResult()).doubleValue();
	}
	public int getFunctorArity() throws ConversionFailedException, IllegalTermException, Exception
	{
	    Token request = owner.sendMessage("()I", spt, "getFunctorArity",
					      new Object[] {});
	    return ((Integer)request.getResult()).intValue();
	}
	public String getFunctorName() throws ConversionFailedException, IllegalTermException, Exception
	{
	    Token request = owner.sendMessage("()Ljava/lang/String;",
					      spt, "getFunctorName",
					      new Object[] {});
	    return (String)request.getResult();
	}
	public long getInteger() throws Exception
	{
	    if (debugging(5)) debugPrintln( "entering JasperTerm.getInteger()");
	    Token request = owner.sendMessage("()J", spt, "getInteger",
					      new Object[] {});
	    if (debugging(10)) debugPrintln( "in JasperTerm.getInteger(), returning");
	    return ((Long)request.getResult()).longValue();
	}
	public Term getList(Term head, Term tail) throws InterruptedException, Exception
	{
	    SPTerm spthead = ((JasperTerm)head).getSPTerm();
	    SPTerm spttail = ((JasperTerm)tail).getSPTerm();
	    Token request = owner.sendMessage("(Lse/sics/jasper/Term;Lse/sics/jasper/Term;)Lse/sics/jasper/Term;",
					      spt, "getList",
					      new Object[] {spthead, spttail});
	    return this;
	}
	public String getListChars() throws Exception
	{
	    Token request = owner.sendMessage("()Ljava/lang/String;",
					      spt, "getListChars",
					      new Object[] {});
	    return (String)request.getResult();
	}
	public String getNumberChars() throws Exception
	{
	    Token request = owner.sendMessage("()Ljava/lang/String;",
					      spt, "getNumberChars",
					      new Object[] {});
	    return (String)request.getResult();
	}
	public Object getObject() throws Exception
	{
// [PD] 3.9 There is no putObject() yet, so this can't be used. Using unify()
//          instead of putObject() (as with the other put<Foo>() methods is
//          not acceptable since we need to create a GlobalRef for the object
//          before it is stored in a prolog term.
// [PD] 3.9.2 JasperProlog.newObjectTerm() now exists, so this code can
//            safely be used.
            Token request = owner.sendMessage("()Ljava/lang/Object;",
					      spt, "getObject",
					      new Object[] {});
 	    return request.getResult();
	}
	public String getString() throws Exception
	{
	    Token request = owner.sendMessage("()Ljava/lang/String;",
					      spt, "getString",
					      new Object[] {});
	    return (String)request.getResult();
	}
	public boolean isAtom() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isAtom",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isAtomic() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isAtomic",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isCompound() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isCompound",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isEmptyList() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isEmptyList",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isFloat() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isFloat",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isInteger() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isInteger",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isList() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isList",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isNumber() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isNumber",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isValid() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isValid",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
	public boolean isVariable() throws Exception
	{
	    Token request = owner.sendMessage("()Z", spt, "isVariable",
					      new Object[] {});
	    return ((Boolean)request.getResult()).booleanValue();
	}
/* We don't want the put methods. Use unify instead.
	public Term putEmptyList()
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putFloat(double value)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putFloat(float value)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
// *** FIX? Perhaps we need putFunctor after all?
	public Term putFunctor(String functor, int arity)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putInteger(int value)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putList()
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putListChars(String string)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putNumberChars(String string)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putObject(Object obj)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putString(String value)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putTerm(Term new_term)
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
	public Term putVariable()
	{
	    return new JasperTerm(new PrologServer());	// *** temp
	}
*/
	public String toString()
//	    If we call SPTerm.toString() (via sendMessage()), this method must
//	    throw Exception. The compiler will complain about that since
//	    Object.toString() does not throw Exception.
//	    Solution: catch the Exception from sendMessage() and return the
//	    result from super.toString (which is Object.toString) with the
//	    exception string appended.
	{
	    if (debugging(5)) debugPrintln("Entering JasperTerm.toString()");
	    try {
		Token request = owner.sendMessage("()Ljava/lang/String;",
						  spt, "toString",
						  new Object[] {});
		return (String)request.getResult();
	    } catch (Exception ex) {
		if (debugging(2)) debugPrintln("Warning: SPTerm.toString() failed. Using Object.toString instead.");
		if (debugging(5)) {
		    debugPrintln("    exception:" + ex);
		    ex.printStackTrace(System.err);
		}
		return super.toString() + ex.toString();
	    }
	}
	public String toString(Term options) throws Exception
	{
	    if (debugging(5)) debugPrintln("Entering JasperTerm.toString(Term options)");
	    if (debugging(5)) debugPrintln("    spt.getClass()==" + spt.getClass());
	    SPTerm sptoptions = ((JasperTerm)options).getSPTerm();
	    Token request = owner.sendMessage("(Lse/sics/jasper/Term;)Ljava/lang/String;",
					     spt, "toString",
					     new Object[] {sptoptions});
	    if (debugging(5)) debugPrintln("Returning from JasperTerm.toString(Term options)");
	    return (String)request.getResult();
	}
        public Term[] toPrologTermArray() throws InterruptedException, Exception
	{
	    Token request = owner.sendMessage("()[Lse/sics/jasper/SPTerm;",
					      spt, "toTermArray",
					      new Object[] {});
	    SPTerm[] spta = (SPTerm[])request.getResult();
	    Term[] ta = new Term[spta.length];
	    for (int i = 0; i < ta.length; i++) {
		ta[i] = new JasperTerm(owner, jasperServer, spta[i]);
	    }
	    return ta;
	}
	public int type() throws Exception
	{
	    Token request = owner.sendMessage("()I", spt, "type",
					      new Object[] {});
	    return ((Integer)request.getResult()).intValue();
	}
	public boolean unify(Term with) throws Exception
	{
	    SPTerm sptwith = ((JasperTerm)with).getSPTerm();
	    Token request = owner.sendMessage("(Lse/sics/jasper/Term;)Z",
					      spt, "unify",
					      new Object[] {sptwith});
	    return ((Boolean)request.getResult()).booleanValue();
	}

	JasperTerm(JasperProlog owner, PrologServer server, SPTerm spt)
	    throws InterruptedException
	{
// [PD] 3.10.2
//	    super(server, server.getSICStus());
	    if (debugging(5)) debugPrintln( "Creating a JasperTerm");
	    this.owner = owner;	// [PD] 3.10.2
	    this.spt = spt;
	}
    }

    /* Meta-method */
    void callMethod(SICStus sp,Token request)
    {
	if (debugging(5)) debugPrintln( "Entering callMethod()");
	try {
	    char returnType = request.getReturnType();
	    if (debugging(5)) debugPrintln( "    return type is " + returnType);
	    switch(returnType) {
	    case 'N': callNewObject(sp, request); break;
	    case 'V': callVoidMethod(sp, request); break;
	    case '[':
	    case 'L': callObjectMethod(sp, request); break;
	    case 'Z': callBooleanMethod(sp, request); break;
	    case 'B': callByteMethod(sp, request); break;
	    case 'C': callCharMethod(sp, request); break;
	    case 'S': callShortMethod(sp, request); break;
	    case 'I': callIntMethod(sp, request); break;
	    case 'J': callLongMethod(sp, request); break;
	    case 'F': callFloatMethod(sp, request); break;
	    case 'D': callDoubleMethod(sp, request); break;
	    default:
		throw new Exception("unknown type: " + returnType);
	    }
	    // Propagate all exceptions!
	    // *** FIX? We should perhaps not propagate InterruptedException?
	} catch (Exception ex) {
	    if (debugging(5)) debugPrint("in callMethod(), caught an Exception ");
	    if (debugging(5)) {
		ex.printStackTrace(System.err);
	    } else {
		if (debugging(5)) debugPrintln(""); // *** ???
	    }
	    request.setResult(java.lang.Void.TYPE);
	    request.setException(ex);
	} 
    }

    void callNewObject(SICStus sp, Token request)
    {
	Object result = sp.spNewObject(request.getObject(),
				       request.getMethodname(),
				       request.getArgs(),
				       request.getTypesig());
	request.setResult(result);
    }

    void callVoidMethod(SICStus sp, Token request)
    {
	sp.spCallVoidMethodByName(request.getObject(),
				  request.getMethodname(),
				  request.getArgs(), request.getTypesig(),
				  request.getStaticP());
	request.setResult(java.lang.Void.TYPE);
    }
    void callObjectMethod(SICStus sp, Token request) throws Exception
    {
	if (debugging(5)) debugPrintln("Entering PrologServer.callObjectMethod()");
//	if (debugging(10)) debugPrintln("    object:     " + request.getObject());
	if (debugging(10)) debugPrintln("    methodname: " + request.getMethodname());
	if (debugging(10)) debugPrintln("    args:       " + request.getArgs());
	if (debugging(10)) debugPrintln("    typesig:    " + request.getTypesig());
	Object result = sp.spCallObjectMethodByName(request.getObject(),
						    request.getMethodname(),
						    request.getArgs(),
						    request.getTypesig(),
						    request.getStaticP());
	if (debugging(5)) debugPrintln("in PrologServer.callObjectMethod(), after native call");
	request.setResult(result);
	if (debugging(5)) debugPrintln("in PrologServer.callObjectMethod(), after setResult()");
    }
    void callBooleanMethod(SICStus sp, Token request) throws Exception
    {
	if (debugging(5)) debugPrintln("Entering PrologServer.callBooleanMethod()");
//	if (debugging(10)) debugPrintln("    object:     " + request.getObject());
	if (debugging(10)) debugPrintln("    methodname: " + request.getMethodname());
	if (debugging(10)) debugPrintln("    args:       " + request.getArgs());
	if (debugging(10)) debugPrintln("    typesig:    " + request.getTypesig());
	boolean result = sp.spCallBooleanMethodByName(request.getObject(),
						      request.getMethodname(),
						      request.getArgs(),
						      request.getTypesig(),
						      request.getStaticP());
	if (debugging(10)) debugPrintln("in PrologServer.callBooleanMethod(), after native call");
	request.setResult(new Boolean(result));
	if (debugging(10)) debugPrintln("in PrologServer.callBooleanMethod(), after setResult()");
    }
    void callByteMethod(SICStus sp, Token request)
    {
	byte result = sp.spCallByteMethodByName(request.getObject(),
						request.getMethodname(),
						request.getArgs(),
						request.getTypesig(),
						request.getStaticP());
	request.setResult(new Byte(result));
    }
    void callCharMethod(SICStus sp, Token request)
    {
	char result = sp.spCallCharMethodByName(request.getObject(),
						request.getMethodname(),
						request.getArgs(),
						request.getTypesig(),
						request.getStaticP());
	request.setResult(new Character(result));
    }
    void callShortMethod(SICStus sp, Token request)
    {
	short result = sp.spCallShortMethodByName(request.getObject(),
						  request.getMethodname(),
						  request.getArgs(),
						  request.getTypesig(),
						  request.getStaticP());
	request.setResult(new Short(result));
    }
    void callIntMethod(SICStus sp, Token request)
    {
	if (debugging(5)) debugPrintln("Entering PrologServer.callIntMethod()");
	if (debugging(10)) debugPrintln("    args: " + request.getArgs());
	if (debugging(10)) {
	    Object[] args = request.getArgs();
	    for (int i = 0; i < args.length; i++) {
		debugPrintln("    args[" + i + "].getClass()==" + args[i].getClass());
	    }
	}
	if (debugging(10)) debugPrintln("    request.getTypesig()==" + request.getTypesig());
	int result = sp.spCallIntMethodByName(request.getObject(),
					      request.getMethodname(),
					      request.getArgs(),
					      request.getTypesig(),
					      request.getStaticP());
	if (debugging(10)) debugPrintln("in PrologServer.callIntMethod(), after native call");
	if (debugging(10)) debugPrintln("    result==" + result);
	request.setResult(new Integer(result));
	if (debugging(10)) debugPrintln("in PrologServer.callIntMethod(), after setResult()");
    }
    void callLongMethod(SICStus sp, Token request)
    {
	if (debugging(5)) debugPrintln("Entering PrologServer.callLongMethod()");
	request.debugPrintToken(10);
	if (debugging(10)) debugPrintln("in PrologServer.callLongMethod(), before native call");
	long result = sp.spCallLongMethodByName(request.getObject(),
						request.getMethodname(),
						request.getArgs(),
						request.getTypesig(),
						request.getStaticP());
	if (debugging(10)) debugPrintln("in PrologServer.callLongMethod(), after native call");
	request.setResult(new Long(result));
	if (debugging(10)) debugPrintln("in PrologServer.callLongMethod(), after setResult()");
    }
    void callFloatMethod(SICStus sp, Token request)
    {
	float result = sp.spCallFloatMethodByName(request.getObject(),
						  request.getMethodname(),
						  request.getArgs(),
						  request.getTypesig(),
						  request.getStaticP());
	request.setResult(new Float(result));
    }
    void callDoubleMethod(SICStus sp, Token request)
    {
	double result = sp.spCallDoubleMethodByName(request.getObject(),
						    request.getMethodname(),
						    request.getArgs(),
						    request.getTypesig(),
						    request.getStaticP());
	request.setResult(new Double(result));
    }

    /* [PD] 3.9 Should not have to be public. */
//    public Server getServer()
    Server getServer()
    {
	return jasperServer;
    }

    static ThreadLocal JasperGlobalCallerStack = new java.lang.ThreadLocal()
	{
	    protected Object initialValue()
	    {
		Stack st = new Stack();
		st.push(null);
		return st;
	    }
	};

    // *** [PD] FIX: This needs more detailed documentation.
    /**
     * Returns the Prolog interface corresponding to the SICStus runtime that
     * called us.
     * If Java is the toplevel, this method returns null,
     */
    public static Prolog getCaller()
    {
	return (Prolog)((Stack)(JasperGlobalCallerStack.get())).peek();
    }

    /**
     * Sets the calling Prolog interface. This is done automatically for each
     * call to the Prolog runtime (e.g. a query or similar), but can also be
     * done explicitly with this method.
     */
// [PD] Does this have to be public? *** FIX: Is it necessary to keep it at all?
//    public static void setCaller(Prolog p)
    static void setCaller(Prolog p)
    {
	((Stack)(JasperGlobalCallerStack.get())).push(p);
    }


    // The arguments to newProlog must match the constructors for
    // class SICStus.
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, null, null)}.
     */
    public static Prolog newProlog() throws InterruptedException
    {
	return newProlog(null, null, null);
    }
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, bootPath, null)}.
     * @param bootPath The path where SICStus should look for its start-up
     * files.
     */
    public static Prolog newProlog(String bootPath) throws InterruptedException
    {
	return newProlog(null, bootPath, null);
    }
    /**
     * Creates a {@link se.sics.jasper.Prolog} interface object. Starts a server
     * thread which will serve that {@link se.sics.jasper.Prolog}. The server
     * thread takes care of all interaction with the Prolog runtime, making sure
     * that all calls to the Prolog runtime will be done from one and the same
     * thread.
     *
     * @param argv Argument vector to the emulator.
     * @param bootPath The path where SICStus should look for its  start-up
     * files.
     * @param savFile A .sav-file to restore. See
     * {@link se.sics.jasper.SICStus#restore}
     */
    public static Prolog newProlog(String[] argv,String bootPath,String savFile)
	throws InterruptedException
    {
	Jasper js = new Jasper(argv, bootPath, savFile);
	return js.jasperProlog;
    }
    /**
     * Multiple threads must synchronize when calling this method.
     *
     *
     */
/* [PD] 3.9 Should not have to be public. */
//    public static Prolog newProlog(SICStus sp) throws InterruptedException
    static Prolog newProlog(SICStus sp) throws InterruptedException
    {
	Jasper js = new Jasper(sp);
	return js.jasperProlog;
    }

    void startServer()
    {
	Thread serverThread = new Thread((PrologServer)jasperServer,
					 "ServerThread");
	serverThread.setDaemon(true);
	serverThread.start();
    }

    Jasper(String[] argv, String bootPath, String savFile)
	throws InterruptedException
    {
	// *** FIX? Remove the counters when non-Beta.
	tokencounter = new Counter();
	queuecounter = new Counter();
	jasperServer = new PrologServer(argv, bootPath, savFile);
	// Order is important here. jasperServer must exist when
	// a new JasperProlog is created.
	jasperProlog = new JasperProlog(jasperServer);
	// Order is important here, too. The server must be running
	// before jasperProlog is initialized.
	startServer(); 
	jasperProlog.initProlog(jasperServer);
    }
    Jasper(SICStus sp) throws InterruptedException
    {
	tokencounter = new Counter();
	queuecounter = new Counter();
	jasperServer = new PrologServer(sp);
	// Order is important here. jasperServer must exist when
	// a new JasperProlog is created.
	jasperProlog = new JasperProlog(jasperServer, sp);
    }

    /* Test Jasper.
	java -Djava.library.path=/usr/local/lib -Dsicstus.path=/usr/local/lib/sicstus-3.8.6 se.sics.jasper.Jasper
    */
    /** This is a small test function. It will try to load Jasper by creating
     * a Jasper object and prints a message if it succeeded.
     */

    public static void main(String argv[])
    {
	try {
	    System.out.print("Trying to start Jasper...");
	    Prolog j = newProlog();
	    System.out.println("OK");
	} catch (Exception ex) {
	    System.err.println("Failed to start Jasper: ");
	    ex.printStackTrace(System.err);
	}
    }

}
