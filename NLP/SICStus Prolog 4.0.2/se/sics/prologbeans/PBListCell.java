/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;
import java.io.IOException;
import java.util.ArrayList;

/**
 * <code>PBListCell</code> is the {@if.java Java}{@if.dotnet .NET} representation of Prolog list
 * cells, i.e. compound terms with the functor ./2.
 */
class PBListCell extends PBCompound
{
    private class PBArrayList
    {
	PBTerm[] termList;

	PBArrayList(PBTerm[] terms)
	{
	    termList = terms;
	}
    }

    private class PBString
    {
	String str;

	PBString(String str)
	{
	    this.str = str;
	}
    }

    private PBArrayList arrayList = null;

    private String cachedString = null;

    PBListCell(PBTerm head, PBTerm tail)
    {
	super(".", new PBTerm[]{head, tail});
    }

    public boolean isListCell()
    {
	return true;
    }

    // There is no Character.isDefined(int) in .NET
    private boolean isUnicode(int c)
    {
	return ((c < 0xD800 || c > 0xDFFF) && c <= 0x10FFFD);
    }

    // There is no StringBuilder.appendCodePoint(int) in .NET
    private void appendCodePoint(StringBuffer sb, int c)
    {
	if (c < 0xFFFF) {
	    sb.append((char)c);
	} else {
	    int c1 = c - 0x10000;
	    int c1h = c1 >> 10; // higher 10 bits of c1
	    int c1l = (c1 << 10) >> 10; // lower 10 bits of c1
	    int u1 = 0xD800;
	    int u2 = 0xDC00;
	    u1 = u1 | c1h;
	    u2 = u2 | c1l;
	    sb.append((char)u1);
	    sb.append((char)u2);
	}
    }

    public boolean isString()
    {
	if (cachedString != null) {
	    return true;
	} else {
	    try {
		cachedString = getString();
		return true;
	    } catch (IllegalStateException ise) {
		cachedString = null; // *** FIXME: is this necessary?
		return false;
	    }	    
	}
    }

    public String getString() // throws IllegalStateException
    {
	if (cachedString == null) {
	    StringBuffer sbuf = new StringBuffer();
	    cachedString = getString(sbuf).toString();
	}
	return cachedString;
    }

//[PD] 4.0.0 Don't use this recursive version. It eats to much stack.
//     private StringBuffer getString(StringBuffer sbuf) // throws IllegalStateException
//     {
//     // Attempt a conversion and cache the result if successful.
//     // A list starting with a PBListCell can be converted to a string if
//     // all head() are one character atoms or integers (in the Unicode range).
// 	PBTerm h = arguments[0]; // head()
// 	if ( h.isAtom() && h.name.length() == 1) { // Is head a one char atom?
// 	    sbuf.append(((PBAtom)h).getName());
// 	} else if ( h.isInteger() ) { 	// Is head an integer?
// 	    // *** FIXME: Perhaps check if this cast truncates.
// 	    int c = (int)h.intValue();
// 	    if (!isUnicode(c)) {
// 		throw new IllegalStateException("not a list of characters");
// 	    }
// 	    appendCodePoint(sbuf,c);
// 	} else {
// 	    throw new IllegalStateException("not a list of characters");
// 	}
// 	PBTerm t = arguments[1]; // tail()
// 	if (t == PBTerm.NIL) {
// 	    return sbuf;
// 	} else {
// 	    if (t.isListCell()) {
// 		return ((PBListCell)t).getString(sbuf);
// 	    } else {
// 		throw new IllegalStateException("not a list of characters");
// 	    }
// 	}
//     }

    // [PD] 4.0.0 Non-recursive version
    private StringBuffer getString(StringBuffer sbuf) // throws IllegalStateException
    {
	PBListCell pbt = this;
	PBTerm h;
	do {
	    h = pbt.arguments[0];
	    if (h.isAtom() && h.name.length() == 1) { // Is head a one char atom?
		sbuf.append(((PBAtom)h).getName());
	    } else if ( h.isInteger() ) { 	// Is head an integer?
		// *** FIXME: Perhaps check if this cast truncates.
		int c = (int)h.intValue();
		if (!isUnicode(c)) {
		    throw new IllegalStateException("not a list of characters");
		}
		appendCodePoint(sbuf,c);
	    } else {
		throw new IllegalStateException("not a list of characters");
	    }
	    PBTerm t = pbt.arguments[1]; // tail()
	    if (t == PBTerm.NIL) {
		return sbuf;
	    } else {
		if (t.isListCell()) {
		    pbt = (PBListCell)t;
		} else {
		    throw new IllegalStateException("not a list of characters");
		}
	    }
	} while (true);
    }

    /**
     * Returns the head of this list cell.
     */
    public PBTerm head()
    {
//	return getArgument(1);
	return arguments[0];
    }

    /**
     * Returns the tail of this list cell.
     */
    public PBTerm tail()
    {
//	return getArgument(2);
	return arguments[1];
    }

    /**
     * Returns the length of this <code>PBListCell</code>.
     * <p>
     */
    public int length()
    {
	return 1 + arguments[1].length();
    }

    // FIXME: possibly redundant when QueryAnswer.getValue har been cleaned up.
    PBTerm getTermAt(int index)
    {
	int i = index -1;
	if (i < 0) {
	    throw new IndexOutOfBoundsException();
	} else if (i == 0) {
	    return arguments[0];
	} else {
	    PBTerm cdr = arguments[1];
	    if (cdr.isEmptyList()) {
		throw new IndexOutOfBoundsException();
	    } else {
		return ((PBListCell)cdr).getTermAt(index - 1);
	    }
	}
    }

    String toPrologString()
    {
	StringBuffer sb = new StringBuffer().append('[');
	sb.append(arguments[0].toPrologString());
	PBTerm t = arguments[1];
	while (!t.isEmptyList()) {
	    sb.append(',');
	    sb.append(t.head().toPrologString());
	    t = t.tail();
	}
	sb.append(']');
	return sb.toString();
    }

    public String toString()
    {
	StringBuffer sb = new StringBuffer().append('[');
	sb.append(arguments[0].toString());
	PBTerm t = arguments[1];
	while (!t.isEmptyList()) {
	    sb.append(',');
	    sb.append(t.head().toString());
	    t = t.tail();
	}
	sb.append(']');
	return sb.toString();
    }

    // FIXME:
    // Do not rely on PBCompound.fastWrite. Write a PBList.fastWrite which
    // writes in fastrw special list notation.
//     void fastWrite(FastWriter writer) throws IOException
//     {
// 	if (arguments != null) {
// 	    for (int i = 0, n = arguments.length; i < n; i++) {
// 		writer.writeList();
// 		arguments[i].fastWrite(writer);
// 	    }
// 	}
// 	if (nextTerm != NIL) {
// 	    nextTerm.fastWrite(writer);
// 	} else {
// 	    writer.writeNIL();
// 	}
//     }

    void fastWrite(FastWriter writer) throws IOException
    {
	if (isString()) {
	    writer.writeString(getString());
	} else {
	    super.fastWrite(writer);
	}
    }

}
