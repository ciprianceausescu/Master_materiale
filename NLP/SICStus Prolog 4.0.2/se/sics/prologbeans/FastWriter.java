/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */

package se.sics.prologbeans;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;

/**
 * <code>FastWriter</code>
 */
class FastWriter {

  // [PD] 3.12.1 UTF-8 is required to make SICStus happy.
  private final static String ENCODING = "UTF8";

  private OutputStream output;
  private boolean isWritingTerm = false;
  private Hashtable variableTable = null;

  public FastWriter(OutputStream output) {
    this.output = output;
  } // FastWriter constructor

  private void initOutput() throws IOException {
    if (!isWritingTerm) {
      isWritingTerm = true;
      output.write(FastParser.VERSION);
    }
  }

  public void writeCompound(String name, int arguments) throws IOException {
    initOutput();
    output.write(FastParser.COMPOUND);
    // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
    output.write(name.getBytes(ENCODING));
    output.write(0);
    output.write(arguments);
  }

  public void writeList() throws IOException {
    initOutput();
    output.write(FastParser.LIST);
  }

  public void writeNIL() throws IOException {
    initOutput();
    output.write(FastParser.NIL);
  }

/* [PD] 3.12.3 Bogus
  public void writeString(String value) throws IOException {
    initOutput();
    output.write(FastParser.STRING);
// [PD] 3.12.2 Strings should not be UTF-8-encoded
//    // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
//    output.write(value.getBytes(ENCODING));
    output.write(value.getBytes("ISO-8859-1"));
    output.write(0);
    output.write(FastParser.NIL);
  }
*/

// [PD] 3.12.3 Correct version which handles character codes > 255
  public void writeString(String value) throws IOException {
    initOutput();
//    writeCharList(value, false); // recursive version
//    output.write(0);
    writeCharList(value);
    output.write(FastParser.NIL);
  }

//[PD] 4.0.0 Don't use this recursive version. It eats to much stack.
//  private void writeCharList(String value, boolean in_compact_list)
//       throws IOException
//   {
//       if (PrologSession.debugging(3)) {
// 	  System.err.println(" writeCharList(): value.length()==" + value.length());
//       }
//       if (value.length() > 0) {
// 	  char car = value.charAt(0);
// 	  String cdr = value.substring(1);
// 	  if (0 < car && car < 256) {
// 	      if (!in_compact_list) { output.write(FastParser.STRING); }
// 	      output.write((int)car);
// 	      writeCharList(cdr, true);
// 	  } else {
// 	      if (in_compact_list) { output.write(0); }
// 	      output.write(FastParser.LIST);
// 	      output.write(FastParser.INTEGER);
// 	      output.write(Integer.toString(car).getBytes());
// 	      output.write(0);
// 	      writeCharList(cdr, false);
// 	  }
//       }
//   }

// [PD] 4.0.0 Non-recursive version.
    private void writeCharList(String value)
	throws IOException
    {
	boolean in_compact_list = false;
	for (int i = 0 ; i < value.length(); i++) {
	    char c = value.charAt(i);
	    if (0 < c && c < 256) {
		if (!in_compact_list) { output.write(FastParser.STRING); }
		output.write((int)c);
		in_compact_list = true;
	    } else {
		if (in_compact_list) { output.write(0); }
		output.write(FastParser.LIST);
		output.write(FastParser.INTEGER);
		output.write(Integer.toString(c).getBytes());
		output.write(0);
		in_compact_list = false;
	    }
	}
	if (in_compact_list) { output.write(0); }
    }

/* [PD] 3.12.3 Bogus
  public void writeString(String value, Term nextTerm) throws IOException {
    initOutput();
    output.write(FastParser.STRING);
// [PD] 3.12.2 Strings should not be UTF-8-encoded
//    // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
//    output.write(value.getBytes(ENCODING));
    output.write(value.getBytes("ISO-8859-1"));
    output.write(0);
    if (nextTerm != PBList.NIL) {
      nextTerm.fastWrite(this);
    } else {
      output.write(FastParser.NIL);
    }
  }
*/

// [PD] 3.12.3 Correct version which handles character codes > 255
  public void writeString(String value, PBTerm nextTerm) throws IOException {
    initOutput();
//    writeCharList(value, false);
    writeCharList(value);
    output.write(0);
    if (nextTerm != PBTerm.NIL) {
      nextTerm.fastWrite(this);
    } else {
      output.write(FastParser.NIL);
    }
  }

  public void writeAtom(String value) throws IOException {
    initOutput();
    output.write(FastParser.ATOM);
    // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
    output.write(value.getBytes(ENCODING));
    output.write(0);
  }

//   public void writeAtomic(PBAtomic atomic) throws IOException {
//     initOutput();
//
//     int type = atomic.getType();
//     switch(type) {
//     case PBAtomic.ATOM:
//       output.write(FastParser.ATOM);
//       break;
//     case PBAtomic.FLOAT:
//       output.write(FastParser.FLOAT);
//       break;
//     case PBAtomic.INTEGER:
//       output.write(FastParser.INTEGER);
//       break;
//     case PBAtomic.VARIABLE:
//       output.write(FastParser.VARIABLE);
//       break;
//     }
//     if (type != PBAtomic.VARIABLE) {
//     // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
//       output.write(atomic.getName().getBytes(ENCODING));
//       output.write(0);
//     } else {
//       if (variableTable == null) {
// 	variableTable = new Hashtable();
//       }
//
//       String variableName = (String) variableTable.get(atomic);
//       if (variableName == null) {
// 	variableName = "" + '_' + variableTable.size();
// 	variableTable.put(atomic, variableName);
//       }
//     // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
//       output.write(variableName.getBytes(ENCODING));
//       output.write(0);
//     }
//   }

    public void writeAtomic(PBAtomic atomic) throws IOException, IllegalStateException
  {
      initOutput();
      
      int type = atomic.getType();
      switch(type)
      {
      case PBAtomic.ATOM:
	  output.write(FastParser.ATOM);
	  break;
      case PBAtomic.FLOAT:
	  output.write(FastParser.FLOAT);
	  break;
      case PBAtomic.INTEGER:
	  output.write(FastParser.INTEGER);
	  break;
      default:
	  throw new IllegalStateException("illegal type: " + type);
      }
      // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
      output.write(atomic.getName().getBytes(ENCODING));
      output.write(0);
  }

  public void writeVariable(PBVariable var) throws IOException
  {
      output.write(FastParser.VARIABLE);
      if (variableTable == null) {
	  variableTable = new Hashtable();
      }	  
      String variableName = (String) variableTable.get(var);
      if (variableName == null) {
	  variableName = "" + '_' + variableTable.size();
	  variableTable.put(var, variableName);
      }
      // [PD] 3.12.1 Correct encoding is required to make SICStus happy.
      output.write(variableName.getBytes(ENCODING));
      output.write(0);
  }

  public void commit() throws IOException {
    output.flush();
    isWritingTerm = false;
    if (variableTable != null) {
      variableTable.clear();
    }
  }

  public void close() throws IOException {
    commit();
    this.output.close();
  }

} // FastWriter
