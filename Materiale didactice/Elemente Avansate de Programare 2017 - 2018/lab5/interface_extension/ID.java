package lab6_lab7.interface_extension;

/**
 * Ca și clasele, interfețele pot fi extinse.
 * O interfață I poate extinde oricâte interfețe, în
 * acest mod adăugându-se la o interfata noi constante
 * şi (anunţuri de) metode.
 */
public interface ID extends IB, IC {

    int c = 99;

    @Override
    void met();
}
