/**
 * Copyright (c) 2005 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;
import java.io.IOException;
import java.math.BigInteger;

/**
 * <code>PBTerm</code> is the {@if.java Java}{@if.dotnet .NET} representations of Prolog terms.
 */
public abstract class PBTerm
{
    /**
     * The <code>PBTerm</code> <code>NIL</code>, with the printname "[]",
     * represents the empty list.
     */
    public final static PBTerm NIL = new PBNil();

    protected final String name;

    /**
     * Creates a new <code>PBTerm</code> instance with the specified name.
     * @throws {@if.java java.lang.}NullPointerException if called with an empty String.
     */
    PBTerm(String name)
    {
	if (name == null) {
	    throw new NullPointerException("PBTerm cannot have a null name");
	}
	this.name = name;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is an atom and
     * <code>false</code> otherwise.
     */
    public boolean isAtom()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a constant
     * (e.g. integer, floating-point number, or atom) and
     * <code>false</code> if this <code>PBTerm</code> is a compound term or
     * variable.
     */
    public boolean isAtomic()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a number and
     * <code>false</code> otherwise.
     */
    public boolean isNumber()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is an integer and
     * <code>false</code> otherwise.
     */
    public boolean isInteger()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is an bignum
     * integer and <code>false</code> otherwise.
     */
    public boolean isBignum()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a
     * floating-point number and <code>false</code> otherwise.
     */
    public boolean isFloat()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a compund
     * term and <code>false</code> otherwise.
     */
    public boolean isCompound()
    {
	return getArity() > 0;
    }

    /** Returns <code>true</code> if this <code>PBTerm</code> is a list cell,
     * i.e. a compound term with the functor ./2, and <code>false</code>
     * otherwise.
     */
    public boolean isListCell()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a proper list
     * and <code>false</code> otherwise.
     */
    public boolean isProperList()
    {
//	return (isListCell() && getArgument(2).isProperList())
//	    || isEmptyList();
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is the empty list
     * and <code>false</code> otherwise.
     */
    public boolean isEmptyList()
    {
	return false;
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a proper
     * list and all of its elements are character codes or one character atoms.
     * Returns <code>false</code> otherwise.
     */
    public boolean isString()
    {
	if (PrologSession.debugging()) {
	    System.err.println("Entering PBTerm.isString()");
	}
	return false;
    }

    /**
     * Returns the head of this <code>PBTerm</code> if it is a list cell, i.e.
     * a compound term with the functor ./2.
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a list cell.
     */
    public PBTerm head()
    {
	throw new IllegalStateException("not a list cell: " + toString());
    }

    /**
     * Returns the tail of this <code>PBTerm</code> if it is a list cell, i.e.
     * a compound term with the functor ./2.
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a list cell.
     */
    public PBTerm tail()
    {
	throw new IllegalStateException("not a list cell: " + toString());
    }

    /**
     * Returns <code>true</code> if this <code>PBTerm</code> is a variable and
     * <code>false</code> otherwise.
     */
    public boolean isVariable()
    {
	return false;
    }

    /**
     * Returns the name of this constant or compound term.
     */
    public String getName()
    {
	return name;
    }

    /**
     * Returns the argument at the specified index. Only compound terms
     * have arguments.<br><strong>Note:</strong> the arguments are indexed
     * from 1 to arity.</br>
     *
     * @param index the (one based) index of the argument
     * @return the argument as a PBTerm
     * @throws {@if.java java.lang.}IllegalStateException if this term is not compound
     */
    public PBTerm getArgument(int index)
    {
	throw new IllegalStateException("not a compound term: " + toString());
    }

    /**
     * If this <code>PBTerm</code> is a proper list, returns its length.
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a proper list.
     */
    public int length()
    {
	throw new IllegalStateException("not a proper list: " + toString());
    }

    /**
     * Returns the number of arguments of this compound term or 0 if this
     * <code>PBTerm</code> is not a compound term.
     */
    public int getArity()
    {
	return 0;
    }

    /**
     * Returns the integer value of this <code>PBTerm</code>.
     *
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not an integer.
     */
    public long intValue()
    {
	throw new IllegalStateException("not an integer: " + toString());
    }

    /**
     * Returns the <code>BigInteger</code> value of this <code>PBTerm</code>.
     *
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a bignum integer.
     * @see BigInteger
     */
    public BigInteger bigIntegerValue()
    {
	throw new IllegalStateException("not an integer: " + toString());
    }

    //   /**
    //    * Returns the integer value of this PBTerm.
    //    *
    //    * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code>
    //    * is not an integer
    //    */
    //   public long longValue()
    //   {
    //     throw new IllegalStateException("not an integer: " + toString());
    //   }

    /**
     * Returns the floating-point value of this PBTerm.
     *
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a number.
     */
    public double floatValue()
    {
	throw new IllegalStateException("not a number: " + toString());
    }

    //   /**
    //    * Returns the floating-point value of this PBTerm.
    //    *
    //    * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code>
    //    * is not a number
    //    */
    //   public double doubleValue()
    //   {
    //     throw new IllegalStateException("not a number: " + toString());
    //   }

    /**
     * If this <code>PBTerm</code> is a proper list and all its elements are
     * small integers (less than 256), returns a string with the list elements
     * as the character codes of the string.
     * @throws {@if.java java.lang.}IllegalStateException if this <code>PBTerm</code> is
     * not a proper list.
     */
    public String getString()
    {
	throw new IllegalStateException("not a proper list: " + toString());
    }

    /**
     * For internal use by PrologBeans.
     *
     * Returns a string representation of this term in a format that can be
     * parsed by a Prolog parser.
     */

    // [PM] FIXME: Should use fastrw-format to send data to Prolog as
    // well. It will be a nightmare trying to produce properly quoted
    // terms in a way that can be read correctly by Prolog. If that is
    // not hard enough try making it work with non-8-bit characters and
    // then start flipping the prolog-flags 'language' (ISO have
    // different quoting rules than SICStus), 'double_quotes' and
    // 'character_escapes'. (Did I mention that I think relying on the
    // prolog reader is a bad idea for the release version :-).
    // [JE] Fixed using writeFast() (toPrologString() not used anymore)

    abstract String toPrologString();

    abstract void fastWrite(FastWriter writer) throws IOException;

    /**
     * Returns a string description of this term.
     */
    public abstract String toString();

    // -------------------------------------------------------------------
    // Static methods to create PBTerm instances
    // -------------------------------------------------------------------

    /**
     * Creates a new <code>PBTerm</code> instance representing a float value.
     */
    static public PBTerm makeTerm(float value)
    {
	return new PBFloat(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a double value.
     */
    static public PBTerm makeTerm(double value)
    {
	return new PBFloat(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing an int value.
     */
    static public PBTerm makeTerm(int value)
    {
	return new PBInteger(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a long value.
     */
    static public PBTerm makeTerm(long value)
    {
	return new PBInteger(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a BigInteger
     * value.
     */
    static public PBTerm makeTerm(BigInteger value)
    {
	return new PBBignum(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a list with
     * the characters, as integer values, in the string argument as its
     * elements.
     */
    static public PBTerm makeTerm(String value)
    {
// *** FIXME: This should create a list. Below code is stopgap.
	throw new UnsupportedOperationException("Not yet implemented");
//	return new PBAtom(value);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a compound
     * term.
     */
    static public PBTerm makeTerm(String name, PBTerm[] arguments)
    {
	// If the compound term being created is a list cell, create an
	// instance of PBListCell instead, making sure that its methods will
	// be called when the instance is used.
	if (name.equals(".") && arguments != null && arguments.length == 2) {
	    return new PBListCell(arguments[0], arguments[1]);
	} else {
	    return new PBCompound(name, arguments);
	}
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing a list cell.
     */
    static public PBTerm makeTerm(PBTerm head, PBTerm tail)
    {
	return new PBListCell(head, tail);
    }

    /**
     * Creates a new <code>PBTerm</code> instance representing an atom.
     */
    static public PBTerm makeAtom(String value)
    {
	if (value.equals("[]")) { return PBAtom.NIL; }
	else { return new PBAtom(value); }
    }


    // -------------------------------------------------------------------
    // Internal utilities
    // -------------------------------------------------------------------

    private int getFirstStuffing(String atom) {
	int len = atom.length();
	if (len == 0) {
	    return 0;
	}
	char c = atom.charAt(0);
	if (c < 'a' || c > 'z') {
	    return 0;
	}

	for (int i = 1; i < len; i++) {
	    c = atom.charAt(i);
	    if (!((c >= 'a' && c <= 'z')
		  || (c >= 'A' && c <= 'Z')
		  || (c >= '0' && c <= '9')
		  || (c == '_'))) {
		return i;
	    }
	}
	return -1;
    }

    protected String stuffAtom(String atom) {
	int start = getFirstStuffing(atom);
	if (start < 0) {
	    // No stuffing needed
	    return atom;
	}

	int len = atom.length();
	char[] buf = new char[start + (len - start) * 2 + 2];
	int index = 1;
	buf[0] = '\'';
	if (start > 0) {
	    atom.getChars(0, start, buf, 1);
	    index += start;
	}
	for (int i = start; i < len; i++) {
	    char c = atom.charAt(i);
	    if (c == '\'') {
		buf[index++] = '\\';
	    }
	    buf[index++] = c;
	}
	buf[index++] = '\'';
	return new String(buf, 0, index);
    }

}
